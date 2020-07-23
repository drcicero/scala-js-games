package util

import org.scalajs.dom.CanvasRenderingContext2D

class MenuGame(message: String, list: Seq[(String, State => Game)])(st: State) extends Game {
  var pointer = 0

  override def update(): List[Game] = {
    val u = st.b2i(st.keyDown(38))
    val d = st.b2i(st.keyDown(40))
    val s = st.b2i(st.keyDown(32))
    if (u+d+s != 0) Sound.pling()
    pointer = (pointer + list.length - u + d) % list.length
    if (s==1) List(list(pointer)._2(st), this)
    else List(this)
  }

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    val fontSize = 20
    val lineHeight = 1.5

    ctx.fillStyle = Color.White
    ctx.font = s"${fontSize}pt PixelOperator"
    ctx.textAlign = "center"

    var yOffset = (st.bounds.y - (list.length + 2) * fontSize * lineHeight)/2
    ctx.fillText(message, st.bounds.x/2, yOffset)
    yOffset += fontSize * lineHeight
    yOffset += fontSize * lineHeight

    list.zipWithIndex.foreach { case ((desc, action), i) =>
      ctx.fillStyle = if (pointer == i) Color.Green else Color.White
      ctx.fillText(desc, st.bounds.x/2, yOffset)
      if (pointer == i) ctx.fillText("["+ " "*25 +"]", st.bounds.x/2, yOffset)
      yOffset += fontSize * lineHeight
    }
    yOffset += fontSize * lineHeight
  }
}
