import de.sciss.midi.Sequence
import markov.MarkovChain
import midi.Midi.{createMidiSequence, readMidiNotes, writeMidiToFile, zip}
import model.{Melody, Note, NoteOrRest, Rest, RhythmicElement}
import shapeless.Sized

object Main extends App {
  val res: String = args match {
    case Array() => "No arguments specified. Please provide MIDI input file!"
    case Array(a) => readMidiNotes(a) match {
      case Left(errorMessage) => "Couldn't parse input file: " + errorMessage
      case Right(inputMelody: Melody) =>
        val notes: List[Note] = inputMelody.toNotes
        val rhythm: List[RhythmicElement] = inputMelody.toRhythm

        val generatedRhythm = MarkovChain.fourthOrder(rhythm)
          .generateSequence(Sized.wrap(rhythm.take(4)),100)

        val generatedMelody = MarkovChain.fourthOrder(notes)
          .generateSequence(Sized.wrap(notes.take(4)), generatedRhythm.count(_.isAudible))

        val midiSequence: Either[String, Sequence] = createMidiSequence(zip(generatedRhythm, generatedMelody))
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