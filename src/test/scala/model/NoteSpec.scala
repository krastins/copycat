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
}
