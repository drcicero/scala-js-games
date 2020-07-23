package games

import org.scalajs.dom
import util.{Color, Game, MessageGame, Point, State}

import scala.collection.mutable
import scala.util.Random

case class AstroLander(st: State) extends Game {
  // to get n different values, take one of m, then one of m-1, then one of m-2, ...
  // then ensure no value is used twice, incrementing each values that have been chosen before
  def nOutOfM(n: Int, m: Int): Seq[Int] = {
    assert(n <= m)
    val result = mutable.Buffer.from((0 until n).map { i => Random.nextInt(m - i) })
    val sorted = mutable.SortedSet[Int]()
    (0 until n).foreach { i =>
      result(i) = sorted.fold(result(i)) { case (acc, ev) => if (ev <= acc) acc + 1 else acc }
      sorted.add(result(i))
    }
    result.toSeq
  }

  val points: Seq[Point] = {
    // choose 3 different values
    val Seq(flat, cliff1, cliff2) = nOutOfM(3, 16).map(_+2)

    var current = 450
    (1 to 19).map { n =>
      current =
        if (n == flat) current
        else if (n == cliff1) current - 150
        else if (n == cliff2) current + 150
        else current + Random.between(-25, +25)
      if (current > st.bounds.y) current = (2 * st.bounds.y - current).toInt
      Point(n * 40, current)
    }
  }

  var craftPos: Point = Point(400, 25)
  var craftVel: Point = Point(0, 0)
  var theta: Double = -math.Pi / 2
  var fuel: Int = 500

  def shipPoints = Seq(
    craftPos + Point(+30, 0).rotate(theta),
    craftPos + Point(-15, -15).rotate(theta),
    craftPos + Point(-15, +15).rotate(theta)
  )

  def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    // clear
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    // hud
    ctx.textAlign = "left"
    ctx.fillStyle = if (craftVel.length < 3) Color.Green else Color.White
    ctx.fillText("Speed: " + (craftVel.length * 10).toInt.toDouble / 10, 20, 50)

    // hud: curr/max fuel
    ctx.strokeStyle = Color.Green
    ctx.fillRect(20, 60, math.max(1, fuel) * 65 / 500, 15)
    ctx.strokeStyle = Color.White
    ctx.strokeRect(20, 60, 65, 15)

    // ground
    (0 until points.length-1).foreach { i =>
      val (p,q) = (points(i), points(i+1))
      ctx.fillStyle = if (tooSteep(p,q)) Color.Red else Color.White
      ctx.beginPath()
      ctx.moveTo(p.x, st.bounds.y)
      ctx.lineTo(p.x, p.y)
      ctx.lineTo(q.x, q.y)
      ctx.lineTo(q.x, st.bounds.y)
      ctx.fill()
    }

    // triangle
    ctx.fillStyle = if (tooSkew && tooFast) Color.Magenta else if (tooFast) Color.Red else if (tooSkew) Color.Blue else Color.White
    ctx.beginPath()
    ctx.moveTo(shipPoints.last.x, shipPoints.last.y)
    shipPoints.foreach(p => ctx.lineTo(p.x, p.y))
    ctx.fill()
    ctx.fillStyle = Color.Yellow
    ctx.fillRect(craftPos.x-1, craftPos.y-1, 2, 2)

    def drawFlame(p: Point, angle: Double): Unit = {
      val offset = math.Pi * 1.25

      def diamond(a: Int, b: Int, c: Int, w: Int): Unit = {
        val width = w * math.Pi / 180

        ctx.strokePath(
          p + Point(a, a).rotate(angle - offset),
          p + Point(b, b).rotate(angle - offset + width),
          p + Point(c, c).rotate(angle - offset),
          p + Point(b, b).rotate(angle - offset - width),
          p + Point(a, a).rotate(angle - offset)
        )
      }

      diamond(5, 15, 25, 15)
      diamond(10, 15, 20, 10)
    }

    ctx.strokeStyle = Color.Red
    if (fuel > 0) {
      if (st.lastKeys(37)) {
        drawFlame(craftPos + Point(-15,0).rotate(theta), theta + math.Pi * 1/2)
        drawFlame(craftPos + Point(+15,0).rotate(theta), theta - math.Pi * 1/2)
      }
      if (st.lastKeys(39)) {
        drawFlame(craftPos + Point(-15,0).rotate(theta), theta - math.Pi * 1/2)
        drawFlame(craftPos + Point(+15,0).rotate(theta), theta + math.Pi * 1/2)
      }
      if (st.lastKeys(40))
        drawFlame(craftPos + Point(-10,0).rotate(theta), theta)
    }
  }

  def update(): List[Game] = {
    if (fuel > 0) {
      val (u,l,d,r) = st.keyDirections()
      if (r+l+d!=0) util.Sound.pling() // noise
      theta = (theta + (r-l) * (Math.PI * 2) / 100 + Math.PI * 2) % (Math.PI * 2)
      craftVel += Point(+0.05 * d, 0).rotate(theta)
      fuel -= Seq(l,r,d).count(_==1)
    }

    craftVel += Point(0, 0.01) // gravity
    craftPos += craftVel

    val hit: Seq[Collide] = points.flatMap { p =>
      val prevIndex = points.lastIndexWhere(_.x < craftPos.x)
      if (prevIndex == -1 || prevIndex == 21) None
      else {
        val prev = points(prevIndex)
        val next = points(prevIndex + 1)
        val height = (craftPos.x - prev.x) / (next.x - prev.x) * (next.y - prev.y) + prev.y
        if (height > craftPos.y) None
        else Some (
          if (tooSteep(prev, next)) Failure("landing area too steep")
          else if (tooSkew) Failure("too much horizontal velocity")
          else if (tooFast) Failure("coming in too fast")
          else Success
        )
      }
    }

    List(
      hit.headOption.fold[Game] {
        AstroLander.this
      } { msg =>
        new MessageGame(msg match {
          case Success =>
            util.Sound.pling() // win
            "You have landed successfully."
          case Failure(reason) =>
            util.Sound.pling() // loose
            "You have crashed your lander: " + reason
        })(st)
      }
    )
  }

  private def tooSteep(p: Point, q: Point): Boolean = math.abs((q.y - p.y) / (q.x - p.x)) > 0.1
  private def tooSkew: Boolean = radiansDiff(theta, Math.PI/2*3) > Math.PI/8

  private def radiansDiff(a: Double, b: Double): Double =
    Math.PI - Math.abs(Math.abs(a - b) - Math.PI)

  private def tooFast: Boolean = craftVel.length > 1
}

trait Collide
case object Success extends Collide
case class Failure(reason: String) extends Collide