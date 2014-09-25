package fpinscala.gettingstarted

object MyModule {
  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  def factorial(x: Int): Int = {
    @annotation.tailrec
    def go(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else go(x - 1, acc * x)

    go(x, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) a
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }

  private def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, x, f(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", 56, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
