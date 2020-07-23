package games

import org.scalajs.dom
import util.{Color, Game, MessageGame, Point, State}

import scala.util.Random

trait Spot
case class Wall(duration: Int) extends Spot
case class Apple(duration: Int, bonus: Int) extends Spot
case object Empty extends Spot

case class Snake(st: State) extends Game {
  // constants
  val blockSize: Int = 10
  val gridDim: Point = Point(40, 30)

  // state
  var frameCount: Int = 0
  var snakeLength: Int = 10
  var headPos: Point = Point(gridDim.x.toInt/2, gridDim.y.toInt/2)
  var headDir: Point = Point(1, 0)
  val grid: Array[Array[Spot]] = {
    val spots: Array[Array[Spot]] = Array.fill(gridDim.x.toInt)(Array.fill(gridDim.y.toInt)(Empty))
    for (i <- 0 until gridDim.x.toInt) {
      spots(i)(0) = Wall(Int.MaxValue)
      spots(i)(gridDim.y.toInt-1) = Wall(Int.MaxValue)
    }
    for (i <- 0 until gridDim.y.toInt) {
      spots(0)(i) = Wall(Int.MaxValue)
      spots(gridDim.x.toInt-1)(i) = Wall(Int.MaxValue)
    }
    spots
  }

  def appleCount: Int = {
    val apples = for {
      col <- grid
      spot <- col
    } yield spot match {
      case Apple(_, _) => 1
      case _ => 0
    }
    apples.sum
  }

  override def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    for {
      i <- 0 until gridDim.x.toInt
      j <- 0 until gridDim.y.toInt
    } grid(i)(j) match {
      case Wall(_) =>
        ctx.fillStyle = Color.White
        ctx.fillRect(i * blockSize, j * blockSize, blockSize, blockSize)
      case Apple(_, x) =>
        ctx.fillStyle = x match {
          case 2 => Color.Red
          case 5 => Color.Yellow
        }
        ctx.fillCircle(i * blockSize + 5, j * blockSize + 5, 5)
      case Empty =>
    }
  }

  override def update(): List[Game] = {
    var nextScene = List[Game](this)
    frameCount += 1

    if (frameCount % 3 == 0) {
      if (math.random > 0.9 + appleCount / 10.0) {
        val (x, y) = (Random.nextInt(gridDim.x.toInt), Random.nextInt(gridDim.y.toInt))
        grid(x)(y) match {
          case Empty =>
            val score = if (Random.nextInt(20) == 0) 5 else 2
            grid(x)(y) = Apple(15 * 25, score)
          case _ =>
        }
      }
      headPos = headPos + headDir

      grid(headPos.x.toInt)(headPos.y.toInt) match {
        case Wall(d) =>
          util.Sound.pling() // loose
          nextScene = List(new MessageGame("You hit a wall!")(st))
        case Apple(_, s) =>
          util.Sound.pling() // good
          snakeLength += s
          grid(headPos.x.toInt)(headPos.y.toInt) = Wall(snakeLength)
        case Empty =>
          grid(headPos.x.toInt)(headPos.y.toInt) = Wall(snakeLength)
      }

      val newDir =
        if (st.keys(37)) Point(-1, 0)
        else if (st.keys(39)) Point(1, 0)
        else if (st.keys(38)) Point(0, -1)
        else if (st.keys(40)) Point(0, 1)
        else headDir

      if (newDir != headDir && newDir + headDir != Point(0, 0)) {
        util.Sound.pling() // hit
        headDir = newDir
      }

      for {
        i <- 0 until gridDim.x.toInt
        j <- 0 until gridDim.y.toInt
      } grid(i)(j) = grid(i)(j) match {
        case Wall(1) | Apple(1, _) => Empty
        case Wall(d) => Wall(d - 1)
        case Apple(d, s) => Apple(d - 1, s)
        case Empty => Empty
      }
    }
    nextScene
  }
}