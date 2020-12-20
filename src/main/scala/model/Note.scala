package model


object NoteName extends Enumeration {
  type NoteName = Value
  // no way to distinguish sharps from flats, because we don't know the scale
  // so let's call all accidentals sharp. this seems to be what Ableton is doing with their piano roll
  val C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B = Value
  def fromInt(value: Int): Option[NoteName] = values.find(_.id == value)
}

object NoteLength extends Enumeration {
  val TicksPerQuarterNote = 96
  type NoteLength = Value
  val Full = Value(384)
  val Half = Value(192)
  val Quarter = Value(96)
  val Eight = Value(48)
  val Sixteenth = Value(24)
  val ThirtySecond = Value(12)
  val SixtyFourth = Value(6)
  def fromInt(ticks: Int): Option[NoteLength] = {
    import math._
    val nearest = if (ticks > 6) {
      val threshold = 0.7 // biased towards the lowest note
      // each note length can be expressed as 6 * 2^n, let's find the closest n
      6 * pow(2, round(log(ticks / 6.0) / log(2) - threshold + 0.5))
    } else 6

    values.find(_.id == nearest)
  }
}

import model.NoteName.NoteName
import model.NoteLength._

sealed trait NoteOrRest {
  val length: NoteLength
}

case class Note(name: NoteName, octave: Int, override val length: NoteLength = Eight) extends NoteOrRest {
  def toMidiPitch: Either[String, Int] = {
    val pitch = name.id + 12 * octave + 24
    if ((0 to 127).contains(pitch)) Right(pitch) else Left(f"Pitch out of range: $pitch")
  }
}

object Note {
  // MIDI note 0 = C-2; MIDI note 24 = C0, aligned with Ableton Live
  def from(midiPitch: Int, midiTicks: Int = Eight.id): Option[Note] =
    for {
      name <- NoteName.fromInt(midiPitch % 12)
      length <- NoteLength.fromInt(midiTicks)
    } yield Note(name, midiPitch / 12 - 2, length)
}

case class Rest(override val length: NoteLength = Quarter) extends NoteOrRest

object Rest {
  def from(midiTicks: Int): Option[Rest] =
    NoteLength.fromInt(midiTicks).map(Rest.apply)
}

case class Melody(sequence: List[NoteOrRest])
