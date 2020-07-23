package util

import org.scalajs.dom

import scala.collection.mutable

class GameStack(canvasName: String, initScenes: List[State => Game]) extends State {
  private[this] val canvas = dom.document.getElementById(canvasName).asInstanceOf[dom.html.Canvas]
  private[this] val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private[this] var scenes = initScenes.map(f => f(this))

  val keys = mutable.Set.empty[Int]
  val lastKeys = mutable.Set.empty[Int]
  val bounds: Point = Point(canvas.width, canvas.height)

  canvas.onfocus = { _: dom.FocusEvent => active = true; util.Sound.audioCtx.resume() }
  canvas.onblur  = { _: dom.FocusEvent => active = false; util.Sound.audioCtx.suspend() }
  canvas.onkeydown = { e: dom.KeyboardEvent =>
    keys.add(e.keyCode)
    if (Seq(32, 37, 38, 39, 40).contains(e.keyCode)) e.preventDefault()
  }
  canvas.onkeyup = { e: dom.KeyboardEvent =>
    keys.remove(e.keyCode)
    if (Seq(32, 37, 38, 39, 40).contains(e.keyCode)) e.preventDefault()
  }

  new MessageGame("Click to focus")(this).draw(ctx)

  var active = false
  def update(): Unit = {
    if (scenes.isEmpty) {
      // nothing
    } else if (active) {
      scenes.head.draw(ctx)
      val nextScenes = scenes.head.update()
      scenes = nextScenes ++ scenes.tail

      lastKeys.clear()
      lastKeys.addAll(keys)
    }
  }
}
