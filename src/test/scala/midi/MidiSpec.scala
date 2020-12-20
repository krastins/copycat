package midi

import de.sciss.midi.{Event, NoteOff, NoteOn}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import midi.Midi.{groupNotes, toNotesWithRests, readMidiNotes}
import model.{Melody, Note, NoteLength, NoteName, NoteOrRest, Rest}
import NoteName._
import NoteLength._

class MidiSpec extends AnyFlatSpec with Matchers {

  val fifthMidi = List(
    Event(48,NoteOn(0, 67, 100)),
    Event(96,NoteOff(0, 67, 0)),
    Event(96,NoteOn(0, 67, 100)),
    Event(144,NoteOff(0, 67, 0)),
    Event(144,NoteOn(0, 67, 100)),
    // notice that we receive note on event before note off, even if the two events happen at the same time
    Event(192,NoteOn(0, 63, 100)),
    Event(192,NoteOff(0, 67, 0)),
    Event(384,NoteOff(0, 63, 0)),
    Event(432,NoteOn(0, 65, 100)),
    Event(480,NoteOff(0, 65, 0)),
    Event(480,NoteOn(0, 65, 100)),
    Event(528,NoteOff(0, 65, 0)),
    Event(528,NoteOn(0, 65, 100)),
    Event(576,NoteOn(0, 62, 100)),
    Event(576,NoteOff(0, 65, 0)),
    Event(960,NoteOff(0, 62, 0))
  )

  val fifthInternal = List(
    Rest(Eight),
    Note(G,3,Eight),
    Note(G,3,Eight),
    Note(G,3,Eight),
    Note(DSharp,3,Half),
    Rest(Eight),
    Note(F,3,Eight),
    Note(F,3,Eight),
    Note(F,3,Eight),
    Note(D,3,Full)
  )

  val orientalMidi = List(
    Event(0, NoteOn(0, 62, 100)),
    Event(48, NoteOff(0, 62, 0)),
    Event(48, NoteOn(0, 62, 100)),
    Event(96, NoteOff(0, 62, 0)),
    Event(96, NoteOn(0, 62, 100)),
    Event(144, NoteOff(0, 62, 0)),
    Event(144, NoteOn(0, 62, 100)),
    Event(192, NoteOn(0, 60, 100)),
    Event(192, NoteOff(0, 62, 0)),
    Event(288, NoteOff(0, 60, 0)),
    Event(288, NoteOn(0, 60, 100)),
    Event(384, NoteOn(0, 57, 100)),
    Event(384, NoteOff(0, 60, 0)),
    Event(480, NoteOff(0, 57, 0)),
    Event(480, NoteOn(0, 57, 100)),
    Event(576, NoteOff(0, 57, 0)),
    Event(576, NoteOn(0, 60, 100)),
    Event(768, NoteOff(0, 60, 0))
  )

  val orientalInternal: List[NoteOrRest] = List(
    Note(D, 3, Eight),
    Note(D, 3, Eight),
    Note(D, 3, Eight),
    Note(D, 3, Eight),
    Note(C, 3, Quarter),
    Note(C, 3, Quarter),
    Note(A, 2, Quarter),
    Note(A, 2, Quarter),
    Note(C, 3, Half)
  )

  "groupNotes" should "group note on and note off events in a tuple" in {
    groupNotes(fifthMidi) shouldBe Right(List(
      (Event(48, NoteOn(0, 67, 100)), Event(96, NoteOff(0, 67, 0))),
      (Event(96, NoteOn(0, 67, 100)), Event(144, NoteOff(0, 67, 0))),
      (Event(144, NoteOn(0, 67, 100)), Event(192, NoteOff(0, 67, 0))),
      (Event(192, NoteOn(0, 63, 100)), Event(384, NoteOff(0, 63, 0))),
      (Event(432, NoteOn(0, 65, 100)), Event(480, NoteOff(0, 65, 0))),
      (Event(480, NoteOn(0, 65, 100)), Event(528, NoteOff(0, 65, 0))),
      (Event(528, NoteOn(0, 65, 100)), Event(576, NoteOff(0, 65, 0))),
      (Event(576, NoteOn(0, 62, 100)), Event(960, NoteOff(0, 62, 0)))
    ))

    groupNotes(orientalMidi) shouldBe Right(List(
      (Event(0, NoteOn(0, 62, 100)), Event(48, NoteOff(0, 62, 0))),
      (Event(48, NoteOn(0, 62, 100)), Event(96, NoteOff(0, 62, 0))),
      (Event(96, NoteOn(0, 62, 100)), Event(144, NoteOff(0, 62, 0))),
      (Event(144, NoteOn(0, 62, 100)), Event(192, NoteOff(0, 62, 0))),
      (Event(192, NoteOn(0, 60, 100)), Event(288, NoteOff(0, 60, 0))),
      (Event(288, NoteOn(0, 60, 100)), Event(384, NoteOff(0, 60, 0))),
      (Event(384, NoteOn(0, 57, 100)), Event(480, NoteOff(0, 57, 0))),
      (Event(480, NoteOn(0, 57, 100)), Event(576, NoteOff(0, 57, 0))),
      (Event(576, NoteOn(0, 60, 100)), Event(768, NoteOff(0, 60, 0)))
    ))
  }

  it should "handle out of order events" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 0)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOff(0, 62, 0)),
      Event(0, NoteOn(0, 62, 100))
    )) shouldBe Right(List(
      (Event(48, NoteOn(0, 62, 100)), Event(96, NoteOff(0, 62, 0))),
      (Event(0, NoteOn(0, 62, 100)), Event(48, NoteOff(0, 62, 0)))
    ))
  }

  it should "fail if there's an unmatched NoteOff message" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 0)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOff(0, 62, 0))
    )) shouldBe Left("Unmatched groups")
  }

  it should "fail if there's an unmatched NoteOn message" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 0)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOn(0, 62, 0))
    )) shouldBe Left("Unmatched groups")
  }

  it should "fail if the pitch doesn't match between a pair of messages" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 0)),
      Event(48, NoteOn(0, 63, 100)),
      Event(96, NoteOff(0, 64, 0)),
      Event(0, NoteOn(0, 65, 100))
    )) shouldBe Left("Unmatched groups")
  }

  "toNotesWithRests" should "handle rests" in {
    toNotesWithRests(groupNotes(fifthMidi)) shouldBe Right(fifthInternal)
  }

  "readMidiNotes" should "parse notes" in {
    val notes = readMidiNotes(getClass.getResource("oriental.mid").getPath)
    notes shouldBe Right(Melody(orientalInternal))
  }

  it should "handle notes with rests" in {
    val notes = readMidiNotes(getClass.getResource("fifth.mid").getPath)
    notes shouldBe Right(Melody(fifthInternal))
  }
}
