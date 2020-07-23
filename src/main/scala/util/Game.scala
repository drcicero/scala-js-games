package util

import org.scalajs.dom

import scala.collection.mutable

trait State {
  def bounds: Point
  def keys: mutable.Set[Int]
  def lastKeys: mutable.Set[Int]
  def keyDown(i: Int): Boolean = keys(i) && !lastKeys(i)
  def keyUp(i: Int): Boolean = !keys(i) && lastKeys(i)

  def b2i(b: Boolean): Int = if (b) 1 else 0
  def keyDirections(): (Int, Int, Int, Int) = {
    val l = b2i(keys(37))
    val r = b2i(keys(39))
    val u = b2i(keys(38))
    val d = b2i(keys(40))
    (u,l,d,r)
  }
}

trait Game {
  def update(): List[Game]
  def draw(ctx: dom.CanvasRenderingContext2D): Unit

  implicit class pimpedContext(val ctx: dom.CanvasRenderingContext2D){
    def fillCircle(x: Double, y: Double, r: Double): Unit = {
      ctx.beginPath()
      ctx.arc(x, y, r, 0, math.Pi * 2)
      ctx.fill()
    }
    def strokePath(points: Point*): Unit = {
      ctx.beginPath()
      ctx.moveTo(points.last.x, points.last.y)
      for(p <- points){
        ctx.lineTo(p.x, p.y)
      }
      ctx.stroke()
    }
  }
}
