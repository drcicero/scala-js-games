package util

object Color {
  def rgb(r: Int, g: Int, b: Int) = s"rgb($r, $g, $b)"
  def hex(hex: String): String = rgb(
    Integer.parseInt(hex.slice(0,2), 16),
    Integer.parseInt(hex.slice(2,4), 16),
    Integer.parseInt(hex.slice(4,6), 16))

  val palette = Seq(
    hex("ffffff"),
    hex("f94144"),
    hex("f3722c"),
    hex("f8961e"),
    hex("f9c74f"),
    hex("90be6d"),
    hex("43aa8b"),
    hex("577590"))

  val White: String   = rgb(255, 255, 255)
  val Red: String     = rgb(255, 0, 0)
  val Green: String   = rgb(0, 255, 0)
  val Blue: String    = rgb(0, 0, 255)
  val Cyan: String    = rgb(0, 255, 255)
  val Magenta: String = rgb(255, 0, 255)
  val Yellow: String  = rgb(255, 255, 0)
  val Black: String   = rgb(0, 0, 0)

  val all = Seq(
    White,
    Red,
    Green,
    Blue,
    Cyan,
    Magenta,
    Yellow,
    Black
  )
}
