package midi

import de.sciss.midi.{Event, NoteOff, NoteOn}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import midi.Midi.{groupNotes, toNotes}
import model.{Note, NoteName}

class MidiSpec extends AnyFlatSpec with Matchers {
  val fifth = List(
    Event(48, NoteOn(0, 67, 90)),
    Event(96, NoteOff(0, 67, 64)),
    Event(96, NoteOn(0, 67, 90)),
    Event(144, NoteOff(0, 67, 64)),
    Event(144, NoteOn(0, 67, 90)),
    // notice that we receive note on event before note off, even if the two events happen at the same time
    Event(192, NoteOn(0, 63, 90)),
    Event(192, NoteOff(0, 67, 64)),
    Event(384, NoteOff(0, 63, 64)))

  val oriental = List(
    Event(0, NoteOn(0, 62, 100)),
    Event(48, NoteOff(0, 62, 64)),
    Event(48, NoteOn(0, 62, 100)),
    Event(96, NoteOff(0, 62, 64)),
    Event(96, NoteOn(0, 62, 100)),
    Event(144, NoteOff(0, 62, 64)),
    Event(144, NoteOn(0, 62, 100)),
    Event(192, NoteOn(0, 60, 100)),
    Event(192, NoteOff(0, 62, 64)),
    Event(288, NoteOff(0, 60, 64)),
    Event(288, NoteOn(0, 60, 100)),
    Event(384, NoteOn(0, 57, 100)),
    Event(384, NoteOff(0, 60, 64)),
    Event(480, NoteOff(0, 57, 64)),
    Event(480, NoteOn(0, 57, 100)),
    Event(576, NoteOff(0, 57, 64)),
    Event(576, NoteOn(0, 60, 100)),
    Event(768, NoteOff(0, 60, 64))
  )

  "groupNotes" should "group note on and note off events in a tuple" in {
    groupNotes(fifth) shouldBe Right(List(
      (Event(48, NoteOn(0, 67, 90)), Event(96, NoteOff(0, 67, 64))),
      (Event(96, NoteOn(0, 67, 90)), Event(144, NoteOff(0, 67, 64))),
      (Event(144, NoteOn(0, 67, 90)), Event(192, NoteOff(0, 67, 64))),
      (Event(192, NoteOn(0, 63, 90)), Event(384, NoteOff(0, 63, 64))))
    )

    groupNotes(oriental) shouldBe Right(List(
      (Event(0, NoteOn(0, 62, 100)), Event(48, NoteOff(0, 62, 64))),
      (Event(48, NoteOn(0, 62, 100)), Event(96, NoteOff(0, 62, 64))),
      (Event(96, NoteOn(0, 62, 100)), Event(144, NoteOff(0, 62, 64))),
      (Event(144, NoteOn(0, 62, 100)), Event(192, NoteOff(0, 62, 64))),
      (Event(192, NoteOn(0, 60, 100)), Event(288, NoteOff(0, 60, 64))),
      (Event(288, NoteOn(0, 60, 100)), Event(384, NoteOff(0, 60, 64))),
      (Event(384, NoteOn(0, 57, 100)), Event(480, NoteOff(0, 57, 64))),
      (Event(480, NoteOn(0, 57, 100)), Event(576, NoteOff(0, 57, 64))),
      (Event(576, NoteOn(0, 60, 100)), Event(768, NoteOff(0, 60, 64)))
    ))
  }

  "groupNotes" should "handle out of order events" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 64)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOff(0, 62, 64)),
      Event(0, NoteOn(0, 62, 100))
    )) shouldBe Right(List(
      (Event(48, NoteOn(0, 62, 100)), Event(96, NoteOff(0, 62, 64))),
      (Event(0, NoteOn(0, 62, 100)), Event(48, NoteOff(0, 62, 64)))
    ))
  }

  "groupNotes" should "fail if there's an unmatched NoteOff message" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 64)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOff(0, 62, 64))
    )) shouldBe Left("Unmatched groups")
  }

  "groupNotes" should "fail if there's an unmatched NoteOn message" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 64)),
      Event(48, NoteOn(0, 62, 100)),
      Event(96, NoteOn(0, 62, 64))
    )) shouldBe Left("Unmatched groups")
  }

  "groupNotes" should "fail if the pitch doesn't match between a pair of messages" in {
    groupNotes(List(
      Event(48, NoteOff(0, 62, 64)),
      Event(48, NoteOn(0, 63, 100)),
      Event(96, NoteOff(0, 64, 64)),
      Event(0, NoteOn(0, 65, 100))
    )) shouldBe Left("Unmatched groups")

  }

  "toNotes" should "convert list of grouped notes to list of internal note representation" in {
    import NoteName._
    toNotes(groupNotes(fifth)) shouldBe Right(List(Note(G,3), Note(G,3), Note(G,3), Note(DSharp,3)))
  }
}
