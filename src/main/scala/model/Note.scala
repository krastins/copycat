package model


sealed abstract class NoteName private (val value: Int)

// TODO: Think about how to better represent notes, since an accidental may have two different names based on the scale
// Maybe I need to infer (or best guess) the scale from the input pattern
// But it's probably just fine to incorrectly call them all sharp
object NoteName {
  case object C extends NoteName(0)
  case object CSharp extends NoteName(1)
  //  case object DFlat extends NoteName(1)
  case object D extends NoteName(value = 2)
  case object DSharp extends NoteName(value = 3)
  //  case object EFlat extends NoteName(value = 3)
  case object E extends NoteName(value = 4)
  case object F extends NoteName(value = 5)
  case object FSharp extends NoteName(value = 6)
  //  case object GFlat extends NoteName(value = 6)
  case object G extends NoteName(value = 7)
  case object GSharp extends NoteName(value = 8)
  //  case object AFlat extends NoteName(value = 8)
  case object A extends NoteName(value = 9)
  case object ASharp extends NoteName(value = 10)
  //  case object BFlat extends NoteName(value = 10)
  case object B extends NoteName(value = 11)

  val values: List[NoteName] = List(C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B)

  def from(value: Int): Option[NoteName] = NoteName.values.find(_.value == value)
}

case class Note(name: NoteName, octave: Int)

object Note {
  def from(midiPitch: Int): Option[Note] =
    NoteName.from(midiPitch % 12).map(Note(_ , midiPitch / 12 -2)) // MIDI note 0 = C-2; MIDI note 24 = C0,
}