package midi

import de.sciss.midi.{Event, NoteOff, NoteOn}
import model.Note

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.util.Try

object Midi {

  def filterNoteEvents(events: Seq[Event]): Seq[Event] =
    events.filter(e => e.message match {
      case NoteOn(_, _, _) | NoteOff(_, _, _) => true
      case _ => false
    })

  private def splitOnMatchingNoteOff(thisPitch: Int, events: List[Event]): (List[Event], List[Event]) =
    events.span(_.message match {
      case NoteOff(_, thatPitch, _) => thisPitch != thatPitch
      case _ => true
    })

  // we need to group the note on and note off events, since they can be out of order in the MIDI file
  @tailrec
  def groupNotes(events: Seq[Event], grouped: List[(Event, Event)] = List.empty): Either[String, List[(Event, Event)]] = {
    events match {
      case Nil => Right(grouped.reverse)
      case event :: rest => event.message match {
        case NoteOn(_, thisPitch, _) =>
          val (preceding, noteOffWithTail) = splitOnMatchingNoteOff(thisPitch, rest)
          noteOffWithTail match {
            case noteOff :: following => groupNotes(preceding ::: following, (event, noteOff) :: grouped)
            case Nil => Left("Unmatched groups")
          }
        case NoteOff(_, _, _) if rest.nonEmpty => groupNotes(rest :+ event, grouped) // not sure if valid scenario
        case NoteOff(_, _, _) if rest.isEmpty => Left("Unmatched groups")
        case _ => groupNotes(rest, grouped)
      }
    }
  }

  def insertRests(pairs: List[(Event, Event)]): List[(Long, Long, Option[Int])] =
    pairs.foldLeft(List.empty[(Long, Long, Option[Int])])((acc, events: (Event, Event)) => {
      val (noteOn, noteOff) = events
      noteOn.message match {
        case NoteOn(_, pitch: Int, _) =>
          val lastTick: Long = acc.headOption.map(_._2).getOrElse(0)
          if (lastTick == noteOn.tick)
            (noteOn.tick, noteOff.tick, Some(pitch)) :: acc
          // let's filter out rests that are smaller than sixteenth note since they probably just mean
          // that the previous Midi note was not drawn to the full length instead of a deliberate pause
          else if (noteOn.tick - lastTick > NoteLength.Sixteenth.id)
            (noteOn.tick, noteOff.tick, Some(pitch)) :: (lastTick, noteOn.tick, None) :: acc
          else {
            acc match {
              // if we filtered a tiny rest, let's add it's length to the previous note
              case head :: tail => (noteOn.tick, noteOff.tick, Some(pitch)) :: head.copy(_2 = noteOn.tick) :: tail
              case Nil => (noteOn.tick, noteOff.tick, Some(pitch)) :: acc
            }
          }
        case _ => acc
      }}).reverse

  def toNotesWithRests(pairs: Either[String, List[(Event, Event)]]): Either[String, List[NoteOrRest]] = {
    val withRests = pairs.map(insertRests)

    withRests.map(_.flatMap { case (fromTick, toTick, noteOrRest) =>  noteOrRest match {
      case Some(pitch) => Note.from(pitch, (toTick - fromTick).toInt)
      case None => Rest.from((toTick - fromTick).toInt)
    }})
  }

  def readMidiNotes(inputFile: String): Either[String, Melody] = {
    import cats.implicits._
    val path = Paths.get(inputFile).toAbsolutePath.toString
    for {
      sequence <- Try(Sequence.read(path)).toEither.leftMap(_.getMessage)
      events = sequence.tracks(0).events.toList
      noteEvents = filterNoteEvents(events)
      grouped = groupNotes(noteEvents)
      sequence <- toNotesWithRests(grouped)
    } yield Melody(sequence)
  }
}
