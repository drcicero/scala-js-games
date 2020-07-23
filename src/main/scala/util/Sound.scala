package util

import org.scalajs.dom.AudioContext
import org.scalajs.dom.OscillatorNode
import org.scalajs.dom.html.Audio
import org.scalajs.dom.raw.GainNode

object Sound {
  def audio(src: String) = {
    val audio = org.scalajs.dom.document.createElement("audio").asInstanceOf[Audio]
    audio.src = src
    audio
  }

  audio("pling.wav") // preload

  def pling(): Unit = audio("pling.wav").play()

  val audioCtx: AudioContext = new AudioContext()
  def play(freq: Double, vol: Double, wave: String, when: Double, dur: Double): Unit = {
    val gainNode: GainNode = audioCtx.createGain()
    gainNode.connect(audioCtx.destination)
    val dt1 = 0.01; gainNode.gain.setTargetAtTime(vol, when - dt1, dt1/5)
    val dt2 = 0.02; gainNode.gain.setTargetAtTime(0.001, when + dur - dt2, dt2/5)

    val oscillator = audioCtx.createOscillator()
    oscillator.connect(gainNode)
    oscillator.`type` = wave
    oscillator.frequency.setValueAtTime(freq, when)
    oscillator.start(when)
    oscillator.stop(when + dur)
  }

  def stutter(seq: Seq[String], n: Int): Seq[String] =
    seq.flatMap { x => x :: List.fill(n-1)("--") }

  val tetrisA: Seq[String] = """
E4 -- B4 C4  D4 -- C4 B4
A4 -- -- C4  E4 -- D4 C4
B4 -- -- C4  D4 -- E4 --
C4 -- A4 --  A4 -- -- --
D4 -- -- F4  A5 -- G4 F4
E4 -- -- C4  E4 -- D4 C4
B4 -- -- C4  D4 -- E4 --
C4 -- A4 --  A4 -- -- --

E4 C4 B4 C4  D4 B4 C4 B4
A4 B4 A4 C4  E4 C4 D4 C4
B4 C4 B4 C4  D4 B4 E4 B4
C4 G#3 A4 G#3 A4 C4 E4 A4
D4 E4 G4 F4  A5 F4 G4 F4
E4 F4 E4 C4  E4 C4 D4 C4
B4 C4 B4 C4  D4 B4 E4 B4
C4 G#3 A4 G#3 A4 E4 C4 A4

E3 -- -- --  C3 -- -- --
D3 -- -- --  B3 -- -- --
C3 -- -- --  A3 -- -- --
G#2 -- -- -- B3 -- -- --
E3 -- -- --  C3 -- -- --
D3 -- -- --  B3 -- -- --
C3 -- E3 --  A4 -- -- --
G#3 -- -- -- -- -- -- --
      """.trim().split("[ \n]+").toSeq

  val tetrisB: Seq[String] = stutter("""
E2 A2 E2 A2 D2 A2 E2 A2
E2 A2 E2 A2 D2 A2 E2 A2
A2 E2 A2 E2 A2 E2 A2 E2
""".trim.split("[ \n]+").toSeq, 8)

  val list: Seq[String] = "A A# B C C# D D# E F F# G G#".split(" ").toSeq
  def freq(n: Int): Double = 440.0 * Math.pow(2, (n-69)/12.0)
  def key(s: String): Int = 9 + list.indexOf(s.init) + 12 * (1+s.last.toString.toInt)

  println(key("A4") == 69, key("A4"))
  println(key("A5") == 81, key("A5"))
  println(freq(key("A4")) == 440, freq(key("A4")))
  println(freq(key("A5")) == 880, freq(key("A5")))
  val step = 0.2 // seconds per note

  def playNotes(vol: Double, melody: Seq[String], startAt: Double, wave: String, transpose: Int = 0): Double = {
    var when = 0.0
    var dur = 0.0
    var pitch = 0.0

    melody foreach { note =>
      if (note == "--") {
        dur += step
      } else {
        if (dur>0) util.Sound.play(pitch, vol, wave, startAt + when, dur - 0.01)
        pitch = freq(key(note) + transpose)
        when += dur
        dur = step
      }
    }
    util.Sound.play(pitch, vol, wave, startAt + when, dur - 0.01)
    when += dur
    when
  }

  def playNotes2(melody: Seq[String], startAt: Double, transpose: Int = 0): Double = {
    var when = 0.0
    melody.sliding(4,4) foreach { case Seq(a,b,c,d) =>
      println(Set(a,c).filter(_ != "--"))
      Set(a,c).filter(_ != "--").foreach { note: String =>
        val pitch = freq(key(note) + transpose * 12)
        util.Sound.play(pitch, 0.8, "sawtooth", startAt + when, 4*step - 0.01)
      }
      when += 4*step
    }
    when
  }
}