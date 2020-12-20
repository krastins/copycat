package model


object NoteName extends Enumeration {
  type NoteName = Value
  // no way to distinguish sharps from flats, because we don't know the scale
  // so let's call all accidentals sharp. this seems to be what Ableton is doing with their piano roll
  val C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B = Value
  def fromInt(value: Int): Option[NoteName] = values.find(_.id == value)
}

import model.NoteName.NoteName

case class Note(name: NoteName, octave: Int, length: NoteLength = Eight) {
  def toMidiPitch: Either[String, Int] = {
    val pitch = name.id + 12 * octave + 24
    if ((0 to 127).contains(pitch)) Right(pitch) else Left(f"Pitch out of range: $pitch")
  }
}

object Note {
  // MIDI note 0 = C-2; MIDI note 24 = C0, aligned with Ableton Live
  def from(midiPitch: Int): Option[Note] =
    NoteName.fromInt(midiPitch % 12).map(Note(_, midiPitch / 12 - 2))
}
