package model


sealed abstract class Name private (val value: Int)

// TODO: Think about how to better represent notes, since an accidental may have two different names based on the scale
// Maybe I need to infer (or best guess) the scale from the input pattern
// But it's probably just fine to incorrectly call them all sharp
object Name {
  case object C extends Name(0)
  case object CSharp extends Name(1)
  //  case object DFlat extends NoteName(1)
  case object D extends Name(value = 2)
  case object DSharp extends Name(value = 3)
  //  case object EFlat extends NoteName(value = 3)
  case object E extends Name(value = 4)
  case object F extends Name(value = 5)
  case object FSharp extends Name(value = 6)
  //  case object GFlat extends NoteName(value = 6)
  case object G extends Name(value = 7)
  case object GSharp extends Name(value = 8)
  //  case object AFlat extends NoteName(value = 8)
  case object A extends Name(value = 9)
  case object ASharp extends Name(value = 10)
  //  case object BFlat extends NoteName(value = 10)
  case object B extends Name(value = 11)

  val values: List[Name] = List(C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B)

  def from(value: Int): Option[Name] = Name.values.find(_.value == value)
}

case class Note(name: Name, octave: Int)

object Note {
  def from(midiPitch: Int): Option[Note] =
    Name.from(midiPitch % 12).map(Note(_ , midiPitch / 12 -2)) // MIDI note 0 = C-2; MIDI note 24 = C0,
}