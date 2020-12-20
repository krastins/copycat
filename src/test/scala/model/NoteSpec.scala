package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.NoteName._

class NoteSpec extends AnyFlatSpec with Matchers {
  
  "note" should "be parsed from MIDI pitch" in {
    Note.from(0) should contain (Note(name = C, octave = -2))
    Note.from(1) should contain (Note(name = CSharp, octave = -2))
    Note.from(2) should contain (Note(name = D, octave = -2))
    Note.from(3) should contain (Note(name = DSharp, octave = -2))
    Note.from(5) should contain (Note(name = F, octave = -2))
    Note.from(8) should contain (Note(name = GSharp, octave = -2))
    Note.from(13) should contain (Note(name = CSharp, octave = -1))
    Note.from(24) shouldBe Some(Note(name = C, octave = 0))
    Note.from(36) shouldBe Some(Note(name = C, octave = 1))
    Note.from(42) shouldBe Some(Note(name = FSharp, octave = 1))
    Note.from(48) shouldBe Some(Note(name = C, octave = 2))
  }

  "toMidiPitch" should "convert list of grouped notes to list of internal note representation" in {
    Note(C, -2).toMidiPitch shouldBe Right(0)
    Note(C, -1).toMidiPitch shouldBe Right(12)
    Note(A, -1).toMidiPitch shouldBe Right(21)
    Note(C, 0).toMidiPitch shouldBe Right(24)
    Note(C, 1).toMidiPitch shouldBe Right(36)
    Note(CSharp, 1).toMidiPitch shouldBe Right(37)
    Note(D, 1).toMidiPitch shouldBe Right(38)
    Note(DSharp, 1).toMidiPitch shouldBe Right(39)
    Note(E, 1).toMidiPitch shouldBe Right(40)
    Note(F, 1).toMidiPitch shouldBe Right(41)
    Note(FSharp, 1).toMidiPitch shouldBe Right(42)
    Note(G, 1).toMidiPitch shouldBe Right(43)
    Note(GSharp, 1).toMidiPitch shouldBe Right(44)
    Note(A, 1).toMidiPitch shouldBe Right(45)
    Note(ASharp, 1).toMidiPitch shouldBe Right(46)
    Note(B, 1).toMidiPitch shouldBe Right(47)
    Note(C, 3).toMidiPitch shouldBe Right(60)
    Note(G, 8).toMidiPitch shouldBe Right(127)
  }

  it should "only work within the 8 bits of MIDI pitch" in {
    import NoteName._
    Note(B, -3).toMidiPitch shouldBe Left("Pitch out of range: -1")
    Note(GSharp, 8).toMidiPitch shouldBe Left("Pitch out of range: 128")
  }

  "fromInt" should  "calculate match the exact " in {
    import NoteLength._
    NoteLength.fromInt(6) shouldBe Some(SixtyFourth)
    NoteLength.fromInt(12) shouldBe Some(ThirtySecond)
    NoteLength.fromInt(24) shouldBe Some(Sixteenth)
    NoteLength.fromInt(48) shouldBe Some(Eight)
    NoteLength.fromInt(96) shouldBe Some(Quarter)
    NoteLength.fromInt(192) shouldBe Some(Half)
    NoteLength.fromInt(384) shouldBe Some(Full)
  }

  it should  "round to nearest known note length" in {
    import NoteLength._
    NoteLength.fromInt(1) shouldBe Some(SixtyFourth)
    NoteLength.fromInt(5) shouldBe Some(SixtyFourth)
    NoteLength.fromInt(7) shouldBe Some(SixtyFourth)
    NoteLength.fromInt(10) shouldBe Some(ThirtySecond)
    NoteLength.fromInt(13) shouldBe Some(ThirtySecond)
    NoteLength.fromInt(20) shouldBe Some(Sixteenth)
    NoteLength.fromInt(28) shouldBe Some(Sixteenth)
    NoteLength.fromInt(44) shouldBe Some(Eight)
    NoteLength.fromInt(51) shouldBe Some(Eight)
    NoteLength.fromInt(99) shouldBe Some(Quarter)
    NoteLength.fromInt(170) shouldBe Some(Half)
    NoteLength.fromInt(200) shouldBe Some(Half)
    NoteLength.fromInt(369) shouldBe Some(Full)
    NoteLength.fromInt(400) shouldBe Some(Full)
  }

  it should "be biased towards the shorter note" in {
    import NoteLength._
    NoteLength.fromInt(23) shouldBe Some(Sixteenth)
    NoteLength.fromInt(24) shouldBe Some(Sixteenth)
    NoteLength.fromInt(45) shouldBe Some(Eight)
    NoteLength.fromInt(45) shouldBe Some(Eight)
    NoteLength.fromInt(21) shouldBe Some(Sixteenth)
    NoteLength.fromInt(26) shouldBe Some(Sixteenth)
    NoteLength.fromInt(51) shouldBe Some(Eight)
    NoteLength.fromInt(55) shouldBe Some(Eight)
    NoteLength.fromInt(25) shouldBe Some(Sixteenth)
    NoteLength.fromInt(31) shouldBe Some(Sixteenth)
    NoteLength.fromInt(58) shouldBe Some(Eight)
    // this note is just an eight note which is held a little longer
    NoteLength.fromInt(76) shouldBe Some(Eight)
    NoteLength.fromInt(277) shouldBe Some(Half)
  }
}
