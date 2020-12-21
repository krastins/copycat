import markov.{MarkovChain, MarkovInput, SeqOrder}
import midi.Midi.{createMidiSequence, readMidiNotes, writeMidiToFile, zip}
import model.{Melody, Note, RhythmicElement}
import cats.syntax.either._
import shapeless.Nat
import SeqOrder.instances._

object Main extends App {
  val res: String = args match {
    case Array() => "No arguments specified. Please provide MIDI input file!"
    case Array(a) => readMidiNotes(a) match {
      case Left(errorMessage) => "Couldn't parse input file: " + errorMessage
      case Right(inputMelody: Melody) =>

        val midiSequence = for {
          notesInput <- MarkovInput.ofOrder[Note, Nat._4](inputMelody.toNotes).leftMap(_.toString)
          rhythmInput <- MarkovInput.ofOrder[RhythmicElement, Nat._1](inputMelody.toRhythm).leftMap(_.toString)
          rhythm = MarkovChain.generateSequenceOfOrder(rhythmInput, 100)
          melody = MarkovChain.generateSequenceOfOrder(notesInput, rhythm.count(_.isAudible))
          midi <- createMidiSequence(zip(rhythm, melody))
        } yield midi

        midiSequence match {
          case Right(sequence) =>
            writeMidiToFile(sequence, "output.mid")
            "Wrote to output.mid"
          case Left(message) => message
        }
    }
    case _ => "Too many arguments specified. Please provide MIDI input file."
  }
  println(res)
}