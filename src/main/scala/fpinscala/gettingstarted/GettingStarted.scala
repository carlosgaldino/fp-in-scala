package fpinscala.gettingstarted

object MyModule {
  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(56))
  }
}
