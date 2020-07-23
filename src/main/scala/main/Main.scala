package main

import games._
import util.{GameStack, MenuGame}

import scala.scalajs.js.timers.setTimeout

object Main {
  def main(args: Array[String]): Unit = {
    val all = Seq(
      "Asteroids"    -> Asteroids,
      "AstroLander"  -> AstroLander,
      "Snake"        -> Snake,
      "Pong"         -> Pong,
      "BrickBreaker" -> BrickBreaker,
      "Tetris"       -> Tetris,
    )

    val game = new GameStack("game", List(
      st => new MenuGame("Select game using arrow keys and space!", all)(st)))

    var musicGeneratedTo = 0.5
    def tick(): Unit = setTimeout(15) {
      if (musicGeneratedTo - util.Sound.audioCtx.currentTime <= util.Sound.step) {
        val dur1 = util.Sound.playNotes(0.1, util.Sound.tetrisA, musicGeneratedTo, "sawtooth")
        val _ = util.Sound.playNotes(0.1, util.Sound.tetrisB, musicGeneratedTo, "sine", 0)
        val _ = util.Sound.playNotes(0.1, util.Sound.tetrisB, musicGeneratedTo, "sine", 7-12)
        musicGeneratedTo += dur1
      }
      game.update()
      tick()
    }
    tick()

  }
}
