package util

import org.scalajs.dom.CanvasRenderingContext2D

class MessageGame(message: String)(st: State) extends Game {
  override def update(): List[Game] =
    if (st.keyDown(32)) List() else List(this)

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    val fontSize = 20

    ctx.fillStyle = Color.White
    ctx.font = s"${fontSize}pt PixelOperator"
    ctx.textAlign = "center"
    ctx.fillText(message, st.bounds.x/2, st.bounds.y/2)
    ctx.fillText("Press space to continue", st.bounds.x/2, st.bounds.y/2 + 2 * fontSize * 1.5)
  }
}
