package games

import org.scalajs.dom
import util.{Color, Game, MessageGame, Point, State}

import scala.collection.mutable
import scala.util.Random

case class Brick(pos: Point, color: String)

case class BrickBreaker(st: State) extends Game {
  // constants
  val bricksPerColumn: Int = 4
  val columnCount: Int = 4
  val borderWidth: Double = 175

  // derived constants
  val fieldWidth: Double = st.bounds.x - borderWidth * 2
  val brickSize: Point = Point(fieldWidth / bricksPerColumn, 30)
  val paddleDims: Point = Point(75, 5)

  // state & reset
  var ballVel: Point = Point(0, 0)
  var ballPos: Point = Point(0, 0)
  var paddlePos: Point = Point(0, 0)
  var paddleVel: Point = Point(0, 0)
  var respawnCounter = 0
  def reset(): Unit = {
    ballVel = Point(0, 0)
    paddlePos = Point(st.bounds.x/2, st.bounds.y-40)
    ballPos = Point(st.bounds.x/2, paddlePos.y - 5)
    paddleVel = Point(0, 0)
    respawnCounter = 60
  }
  reset()

  // persistent state
  var ballsLeft: Int = 3
  val bricks: mutable.Set[Brick] = {
    val colors = Color.palette
    val bricks = scala.collection.mutable.Set.empty[Brick]
    for {
      i <- 0 until (fieldWidth / brickSize.x).toInt
      j <- 3 to 3+columnCount
    } bricks.add(Brick(Point(i * brickSize.x + borderWidth, j * brickSize.y), colors((i+j) % colors.length)))
    bricks
  }

  def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    // clear
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    // border
    ctx.fillStyle = Color.White
    ctx.strokeStyle = Color.White
    ctx.strokeRect(0, 0, borderWidth-1, st.bounds.y)
    ctx.strokeRect(st.bounds.x - borderWidth + 1, 0, borderWidth-1, st.bounds.y)

    // hud
    ctx.textAlign = "left"
    ctx.fillText("Balls Left: " + ballsLeft, st.bounds.x - borderWidth + 10 + 10, 4 * st.bounds.y / 5 - 10)
    for (i <- 1 to ballsLeft) ctx.fillCircle(st.bounds.x - borderWidth + 10 + i * 15, 4 * st.bounds.y / 5 + 10, 5)

    // ball
    ctx.fillCircle(ballPos.x - 5, ballPos.y - 5, 5)

    // bricks
    for (brick <- bricks) {
      ctx.fillStyle = brick.color.replace("255", "128")
      ctx.fillRect(brick.pos.x, brick.pos.y, brickSize.x, brickSize.y)
      ctx.fillStyle = brick.color
      ctx.beginPath()
      ctx.moveTo(brick.pos.x + 1, brick.pos.y + brickSize.y - 1)
      ctx.lineTo(brick.pos.x + 1, brick.pos.y + 1)
      ctx.lineTo(brick.pos.x + brickSize.x - 1, brick.pos.y + 1)
      ctx.stroke()
    }

    // paddle
    ctx.fillStyle = Color.White.replace("255", "128")
    val c1 = paddlePos - paddleDims/2
    ctx.fillRect(c1.x, c1.y, paddleDims.x, paddleDims.y)
    ctx.fillStyle = Color.White
    ctx.strokeRect(c1.x, c1.y, paddleDims.x, paddleDims.y)
  }

  def update(): List[Game] = {
    var result = List[Game](this)
    if (respawnCounter > 0) {
      respawnCounter -= 1
    } else if (respawnCounter == 0) {
      util.Sound.pling() // respawn
      ballVel = Point(3.5 * (Random.nextInt(2) - 0.5) * 2, -3.5)
      respawnCounter -= 1
    } else {
      paddleVel = Point(0, 0)
      if (st.keys(37)) paddleVel = Point(-8, 0)
      if (st.keys(39)) paddleVel = Point(8, 0)

      paddlePos = paddlePos + paddleVel
      paddlePos = paddlePos.copy(x =
        math.min(math.max(paddlePos.x, paddleDims.x / 2 + borderWidth), st.bounds.x - paddleDims.x / 2 - borderWidth))
      ballPos = ballPos + ballVel
      if (ballPos.x + 5 > st.bounds.x - borderWidth) {
        util.Sound.pling() // hit
        ballVel = ballVel.copy(x = -math.abs(ballVel.x))
      } else if (ballPos.x - 5 < borderWidth) {
        util.Sound.pling() // hit
        ballVel = ballVel.copy(x = math.abs(ballVel.x))
      } else if (ballPos.y - 5 < 0) {
        util.Sound.pling() // hit
        ballVel = ballVel.copy(y = math.abs(ballVel.y))
      } else if (ballPos.y > st.bounds.y) {
        ballsLeft -= 1
        if (ballsLeft >= 0) {
          util.Sound.pling() // bad
          reset()
        } else {
          util.Sound.pling() // loose
          result = List(new MessageGame("You've run out of balls!")(st))
        }
      } else {
        if (ballPos.within(paddlePos - paddleDims/2, paddlePos + paddleDims/2, Point(5, 5))) {
          util.Sound.pling() // hit
          ballVel = ballVel.copy(
            x = ballVel.x + paddleVel.x / 8,
            y = -math.abs(ballVel.y))
        }
        for (brick <- bricks) {
          val points = Seq(
            brick.pos,
            brick.pos + Point(brickSize.x, 0),
            brick.pos + brickSize,
            brick.pos + Point(0, brickSize.y)
          )
          val lines = Seq(
            (points(0), points(1), (p: Point) => p.copy(y = -math.abs(p.y))),
            (points(1), points(2), (p: Point) => p.copy(x = math.abs(p.x))),
            (points(2), points(3), (p: Point) => p.copy(y = math.abs(p.y))),
            (points(3), points(0), (p: Point) => p.copy(x = -math.abs(p.x)))
          )
          var hit = false
          for ((p1, p2, func) <- lines if !hit) {
            val extent = (ballPos - p1) * (p2 - p1) / (p2-p1).length
            val perpDist = math.sqrt((ballPos - p1).lengthSquared - extent * extent)
            if (!hit && extent > 0 && extent < (p2-p1).length && perpDist < 5){
              ballVel = func(ballVel)
              bricks.remove(brick)
              util.Sound.pling() // good
              hit = true
            }
          }
          for (p <- points if !hit) {
            val delta = ballPos - p
            if (delta.length < 5) {
              val impulse = delta * (ballVel * delta) / delta.lengthSquared
              ballVel = ballVel - impulse * 2
              bricks.remove(brick)
              util.Sound.pling() // good
              hit = true
            }
          }

        }

        if (bricks.isEmpty) {
          util.Sound.pling() // win
          result = List(new MessageGame("Success! You've destroyed all the bricks!")(st))
        }
      }
    }
    result
  }
}