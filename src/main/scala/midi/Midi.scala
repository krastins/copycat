package midi

import de.sciss.midi.{Event, NoteOff, NoteOn}
import model.Note

import scala.annotation.tailrec

object Midi {
  val PATH: String = getClass.getResource("").getPath

  def filterNoteEvents(events: Seq[Event]): Seq[Event] =
    events.filter(e => e.message match {
      case NoteOn(_, _, _) | NoteOff(_, _, _) => true
      case _ => false
    })

  // This is a little bit of a monstrosity, but so is MIDI
  // TODO: Break up the nested cases into smaller functions
  @tailrec
  def groupNotes(events: Seq[Event], grouped: List[(Event, Event)] = List()): Either[String, List[(Event, Event)]] = {
    events match {
      case Nil => Right(grouped.reverse)
      case event :: rest => event.message match {
        case NoteOn(_, thisPitch, _) =>
          val (preceding, maybeEvents) = rest.span(_.message match {
            case NoteOff(_, thatPitch, _) => thisPitch != thatPitch
            case _ => true
          })
          maybeEvents match {
            case noteOff :: following => groupNotes(preceding ::: following, (event, noteOff) :: grouped)
            case Nil => Left("Unmatched groups")
          }
        case NoteOff(_, _, _) if rest.nonEmpty => groupNotes(rest :+ event, grouped) // not sure if valid scenario
        case NoteOff(_, _, _) if rest.isEmpty => Left("Unmatched groups")
        case _ => groupNotes(rest, grouped)
      }
    }
  }

  def toNote(events: (Event, Event)): Option[Note] = {
    events._1.message match {
      case NoteOn(_, pitch, _) => Note.from(pitch)
      case _ => None
    }
  }
}