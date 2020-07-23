package games

import org.scalajs.dom
import util.{Color, Game, MessageGame, Point, State}

import scala.util.Random

class Paddle(var pos: Point, var dims: Point, var direction: Point, val face: Double)

case class Pong(st: State) extends Game {
  var (ballPos, ballVel) = initBall
  val leftPaddle = new Paddle(Point(40, st.bounds.y/2), Point(5, 75), Point(0, 0), 0.5)
  val rightPaddle = new Paddle(Point(st.bounds.x - 40, st.bounds.y/2), Point(5, 75), Point(0, 0), -0.5)

  var leftScore = 0
  var rightScore = 0
  var respawnCounter = 60
  var aiCounter = 0

  def moveAI(): Unit = {
    val targetY = {
      val padelX = rightPaddle.pos.x
      val distance = padelX - ballPos.x
      val gradient = ballVel.y / ballVel.x
      distance * gradient + ballPos.y
    }
    rightPaddle.direction = Point(0, 0)
    if (math.abs(rightPaddle.pos.x - ballPos.x) < ballVel.x * 45 + 25 && ballVel.x > 0)
      if (targetY > rightPaddle.pos.y + rightPaddle.dims.y / 2) {
        if (math.abs(targetY - rightPaddle.pos.y - (rightPaddle.dims.y / 2)) > rightPaddle.dims.y / 2 - 10)
          rightPaddle.direction += Point(0, 8)
      } else if (targetY < rightPaddle.pos.y + rightPaddle.dims.y / 2)
        if (math.abs(targetY - rightPaddle.pos.y - (rightPaddle.dims.y / 2)) > rightPaddle.dims.y / 2 - 10)
          rightPaddle.direction -= Point(0, 8)
  }

  def initBall: (Point, Point) = (
    Point(400, 300),
    Point(8 * (Random.nextInt(2) - 0.5), 8 * (Random.nextInt(2) - 0.5))
  )

  def doPaddleCollision(paddle: Paddle): Unit = {
    val corner1 = paddle.pos - paddle.dims * paddle.face
    val corner2 = paddle.pos + paddle.dims * paddle.face
    if (ballPos.within(corner2, corner1, extra = Point(5, 5))) {
      util.Sound.pling() // hit
      ballVel = ballVel.copy(
        x = 2 * paddle.face * math.abs(ballVel.x),
        y = ballVel.y + paddle.direction.y / 8
      )
    }
  }

  override def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    ctx.fillStyle = Color.White
    ctx.strokeStyle = Color.White

    ctx.fillCircle(ballPos.x, ballPos.y, 5)

    ctx.fillText("Score (First 3 points is Winner)", 5 * st.bounds.x / 10, st.bounds.y / 8)
    ctx.fillText(leftScore.toString, 5 * st.bounds.x / 10 - 20, st.bounds.y / 8 + 15)
    ctx.fillText(rightScore.toString, 5 * st.bounds.x / 10 + 20, st.bounds.y / 8 + 15)

    for (p <- Seq(leftPaddle, rightPaddle))
      ctx.strokePath(
        Point(p.pos.x + p.dims.x * p.face, p.pos.y - p.dims.y * p.face),
        Point(p.pos.x + p.dims.x * p.face, p.pos.y + p.dims.y * p.face),
        Point(p.pos.x - p.dims.x * p.face, p.pos.y + p.dims.y * p.face)
      )
  }

  override def update(): List[Game] = {
    leftPaddle.direction = Point(0, 0)
    if (st.keys(38)) leftPaddle.direction -= Point(0, 8)
    if (st.keys(40)) leftPaddle.direction += Point(0, 8)

    moveAI()
    ballPos += ballVel
    if (ballPos.y <= 0) {
      util.Sound.pling() // hit
      ballVel = ballVel.copy(y = math.abs(ballVel.y))
    }
    if (ballPos.y >= st.bounds.y) {
      util.Sound.pling() // hit
      ballVel = ballVel.copy(y = -math.abs(ballVel.y))
    }
    if (ballPos.x < 0) {
      util.Sound.pling() // bad
      rightScore += 1
      respawnCounter = 100
    }
    if (ballPos.x > st.bounds.x) {
      util.Sound.pling() // good
      leftScore += 1
      respawnCounter = 100
    }
    if (respawnCounter > 0) {
      val (a, b) = initBall
      ballPos = a
      ballVel = b
      respawnCounter -= 1
    } else if (respawnCounter == 0) {
      util.Sound.pling() // good
      respawnCounter = -1
    }
    for (p <- Seq(leftPaddle, rightPaddle)) {
      doPaddleCollision(p)
      p.pos += p.direction
      p.pos = p.pos.copy(y = math.max(math.min(p.pos.y, st.bounds.y - p.dims.y / 2), p.dims.y / 2))
    }

    if (leftScore == 3) {
      util.Sound.pling() // win
      List(new MessageGame(s"Score $leftScore : $rightScore; left won")(st))
    } else if (rightScore == 3) {
      util.Sound.pling() // loose
      List(new MessageGame(s"Score $leftScore : $rightScore; left won")(st))
    } else
      List(this)
  }
}