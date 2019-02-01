package progfun.week1

object Sqrt {

  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean = {
      abs(guess * guess - x) / x < 1e-14
    }

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(guess = 1.0)
  }
}

object Run extends App {

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc else loop(acc * n, n - 1)
    loop(1, n)
  }

  println(factorial(10))

}
