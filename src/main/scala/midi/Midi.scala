package midi

import de.sciss.midi.{Event, NoteOff, NoteOn, Sequence, TickRate, Track}
import model.NoteLength.NoteLength
import model.{Melody, Note, NoteLength, NoteOrRest, Rest, RhythmicElement}

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.util.Try

object Midi {
  val Bpm = 120
  val Velocity = 100

  // this is supposed to be a constant in the context of this this class, and an implicit for de.sciss.midi.Track
  implicit val tickRate: TickRate = TickRate.apply(NoteLength.TicksPerQuarterNote * Bpm / 60)

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

  def midiNoteLength(noteLength: NoteLength): Long = {
    val ticksPerSecond = tickRate.value
    val ticksPerQuarterNote = ticksPerSecond * 60 / Bpm
    (noteLength.id.toDouble / NoteLength.TicksPerQuarterNote * ticksPerQuarterNote).toLong
  }

  def zip(rhythm: List[RhythmicElement], notes: List[Note]): List[NoteOrRest] =
    rhythm.foldLeft((List.empty[NoteOrRest], notes))((acc, element) => {
      val (result, notes) = acc
      element match {
        case RhythmicElement(length, true) => (notes.head.copy(length = length) :: result, notes.drop(1))
        case RhythmicElement(length, false) => (Rest(length) :: result, notes)
      }
    })._1

  def midiEventsFrom(notes: List[NoteOrRest]): Either[String, List[Event]] = {
    import cats.implicits._
    val acc = Either.right[String, (List[Event], Long)]((List.empty[Event], 0))
    notes.foldLeft(acc)((res: Either[String, (List[Event], Long)], noteOrRest: NoteOrRest) => {
      res.flatMap { case (events: List[Event], restTick: Long) =>
        val lastTick = List[Long](events.headOption.map(_.tick).getOrElse(0), restTick).max
        noteOrRest match {
          case note@Note(_, _, _) =>
            val ticks = lastTick + midiNoteLength(note.length)
            note.toMidiPitch.map(pitch =>
              (Event(ticks, NoteOff(0, pitch, 0)) :: Event(lastTick, NoteOn(0, pitch, Velocity)) :: events, ticks))
          case rest@Rest(_) =>
            val delta = lastTick + midiNoteLength(rest.length)
            Right(events, delta)

        }
      }
    }).map { case (events, _) => events.reverse }
  }

  def createMidiSequence(notes: List[NoteOrRest]): Either[String, Sequence] = {
    import cats.implicits._
    for {
      eventList <- midiEventsFrom(notes)
      events = eventList.toIndexedSeq
      lastTick = events.map(_.tick).max
      track = Track(events, lastTick + tickRate.value.toLong) // pad with extra second
      sequence = Sequence(Vector(track))
    } yield sequence
  }

  def writeMidiToFile(sequence: Sequence, path: String): Unit =
    sequence.write(Paths.get(path).toAbsolutePath.toString)
}
