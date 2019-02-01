package progfun.week2
import scala.annotation.tailrec

object HOF {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)
  }
}

object Run extends App {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)

    loop(a, acc = 0)
  }

  val id = (x: Int) => x
  val cube = (x: Int) => x * x * x

  val sumCubes = (a: Int, b: Int) => sum(cube)(a, b)

  val to20: (Int => Int) => Int = sum(_)(0, 20)
  val toX = (f: Int => Int) => (x: Int) => sum(f)(0, x)

  def alsoToX(f: Int => Int)(x: Int): Int = sum(f)(0, x)

  @tailrec
  val fac: Int => Int = (n: Int) => if (n == 0) 1 else n * fac(n - 1)

  println(to20(fac))
  println(to20(cube))

  println(toX(cube)(100))
}
