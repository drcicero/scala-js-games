package games

import org.scalajs.dom.CanvasRenderingContext2D
import util.{Color, Game, MessageGame, Point, State}

import scala.util.Random

class Position(var filled: String = Color.Black)

case class Tetris(st: State) extends Game {
  // constants
  val minBorder = 175
  val gridDims: Point = Point(10, 20)
  val blockWidth: Int = Math.min(
    (st.bounds.x - minBorder * 2) / gridDims.x,
    st.bounds.y / gridDims.y
  ).toInt
  val border: Double = (st.bounds.x - blockWidth * gridDims.x) / 2

  val pieces = Seq(
    Seq(
      Array(0, 1, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 1, 0, 0)
    ),
    Seq(
      Array(1, 1),
      Array(1, 1)
    ),
    Seq(
      Array(1, 1, 0),
      Array(0, 1, 1),
      Array(0, 0, 0)
    ),
    Seq(
      Array(0, 1, 1),
      Array(1, 1, 0),
      Array(0, 0, 0)
    ),
    Seq(
      Array(0, 1, 0),
      Array(1, 1, 1),
      Array(0, 0, 0)
    ),
    Seq(
      Array(0, 1, 0),
      Array(0, 1, 0),
      Array(1, 1, 0)
    ),
    Seq(
      Array(0, 1, 0),
      Array(0, 1, 0),
      Array(0, 1, 1)
    )
  )

  def iterator(piece: Seq[Array[Int]], offset: Point = Point(0, 0)): Seq[(Int, Int)] = for {
    i <- piece.indices
    j <- piece.head.indices
    if piece(i)(j) != 0
  } yield (i + offset.x.toInt, j + offset.y.toInt)

  // state
  var moveCount: Int = 0
  var keyCount: Int = 0
  var linesCleared: Int = 0
  var nextPiece: Int = Random.nextInt(pieces.length)
  var currentPiece: Int = Random.nextInt(pieces.length)
  var piecePos: Point = Point(gridDims.x/2, 0)
  val grid: Array[Array[Position]] = Array.fill(gridDims.x.toInt, gridDims.y.toInt)(new Position)

  assert(Color.palette.length >= pieces.length)

  def rotate(p: Seq[Array[Int]]): Unit = {
    val w = p.length
    val h = p.head.length
    val center = Point(w - 1, h - 1) / 2
    val out = Seq.fill(w)(Array.fill(h)(0))
    for { i <- 0 until w; j <- 0 until h } {
      val centered = Point(i, j) - center
      val rotated = Point(centered.y * -1, centered.x * 1) + center
      out(rotated.x.toInt)(rotated.y.toInt) = p(i)(j)
    }
    for {i <- 0 until w; j <- 0 until h}
      p(i)(j) = out(i)(j)
  }

  def findCollisions(offset: Point): Seq[Unit] = {
    val pts = iterator(pieces(currentPiece), piecePos).toArray
    for {
      index <- pts.indices
      (i, j) = pts(index)
      newPt = Point(i, j) + offset
      if !newPt.within(Point(0, 0), gridDims) || grid(newPt.x.toInt)(newPt.y.toInt).filled != Color.Black
    } yield {}
  }

  def moveDown(): List[Game] = {
    val collisions = findCollisions(Point(0, 1))
    val pts = iterator(pieces(currentPiece), piecePos).toArray
    if (collisions.isEmpty) {
      piecePos += Point(0, 1)
      List(this)
    } else {
      util.Sound.pling()
      for (index <- pts.indices) {
        val (i, j) = pts(index)
        grid(i)(j).filled = Color.palette(currentPiece)
      }
      currentPiece = nextPiece
      nextPiece = Random.nextInt(pieces.length)
      piecePos = Point(gridDims.x / 2, 0)
      if (findCollisions(Point(0, 0)).nonEmpty) {
        util.Sound.pling() // loose
        List(new MessageGame("The board has filled up!")(st))
      } else
        List(this)
    }
  }

  override def update(): List[Game] = {
    if (st.keyDown(37) && findCollisions(Point(-1, 0)).isEmpty) piecePos += Point(-1, 0)
    if (st.keyDown(39) && findCollisions(Point(1, 0)).isEmpty) piecePos += Point(1, 0)
    if (st.keyDown(38)) {
      rotate(pieces(currentPiece))
      if (findCollisions(Point(0, 0)).nonEmpty)
        for (i <- 0 until 3) rotate(pieces(currentPiece))
    }
    var result = if (st.keys(40)) moveDown() else List(this)

    if (moveCount > 0) moveCount -= 1
    else {
      moveCount = 15
      result = moveDown()
    }

    def row(i: Int): Seq[Position] = (0 until gridDims.x.toInt).map(j => grid(j)(i))
    var remaining = for {
      i <- (gridDims.y.toInt-1 to 0 by -1).toList
      if !row(i).forall(_.filled != Color.Black)
    } yield i

    for (i <- gridDims.y.toInt-1 to 0 by -1) remaining match {
      case first :: rest =>
        remaining = rest
        for ((oldS, newS) <- row(i).zip(row(first)))
          oldS.filled = newS.filled
      case _ =>
        util.Sound.pling() // good
        linesCleared += 1
        for (s <- grid(i)) s.filled = Color.Black
    }
    result
  }

  override def draw(ctx: CanvasRenderingContext2D): Unit = {
    // clear
    ctx.fillStyle = Color.Black
    ctx.fillRect(0, 0, st.bounds.x, st.bounds.y)

    // hud
    ctx.textAlign = "left"
    ctx.fillStyle = Color.White
    ctx.fillText("Lines Cleared: " + linesCleared, border + (gridDims.x+1) * blockWidth, 100)
    ctx.fillText("Next Block", border + (gridDims.x+1) * blockWidth, 170)

    // grid
    def fillBlock(i: Int, j: Int, color: String): Unit = {
      ctx.fillStyle = color
      ctx.fillRect(border + i * blockWidth, 0 + j * blockWidth, blockWidth-1, blockWidth-1)
    }
    for {
      i <- 0 until gridDims.x.toInt
      j <- 0 until gridDims.y.toInt
    } fillBlock(i, j, grid(i)(j).filled)

    // falling
    def draw(p: Int, pos: Point, external: Boolean): Unit = {
      val pts = iterator(pieces(p), pos)
      for (index <- pts.indices) {
        val (i, j) = pts(index)
        if (Point(i, j).within(Point(0, 0), gridDims) || external) fillBlock(i, j, Color.palette(p))
      }
    }
    draw(currentPiece, piecePos, external = false)
    draw(nextPiece, Point(gridDims.x+1, 7), external = true)

    // border
    ctx.strokeStyle = Color.White
    ctx.strokePath(
      Point(border, 0),
      Point(border, st.bounds.y)
    )
    ctx.strokePath(
      Point(st.bounds.x - border, 0),
      Point(st.bounds.x - border, st.bounds.y)
    )
  }
}