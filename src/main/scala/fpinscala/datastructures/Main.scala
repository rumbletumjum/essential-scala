package fpinscala.datastructures

import fpinscala.datastructures.List._

object Main extends App {
  val reverse = (s: String) => s.reverse
  val toUpper = (s: String) => s.toUpperCase
  val appendBar = (s: String) => s + "bar"

  def decorate[A, B](f: (A, B) => B)(a: A, b: B): B = {
    println(s"A = $a B = $b f(a,b) = ${f(a, b)}")
    f(a, b)
  }

  val sum = (a: Int, b: Int) => a + b
  val div = (a: Double, b: Double) => a / b
  val decorateFn = decorate(div) _

  val division = foldLeft(List(4, 2), 16)(_ / _)
  val rightDiv = foldRight(List(16, 4, 2), 1)(_ / _)

  println(division)
  println(rightDiv)
}
