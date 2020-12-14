package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NoteSpec extends AnyFlatSpec with Matchers {

  import NoteName._

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
}