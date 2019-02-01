package org.ron

import fpinscala.datastructures.Cons
import fpinscala.datastructures.List
import fpinscala.datastructures.Nil

trait Show[-A] {
  def show(a: A): String
}

object Show {
  def show[A: Show](a: A): String = implicitly[Show[A]].show(a)

  implicit def intCanShow: Show[Int] = _.toString

  implicit def listCanShow[A]: Show[List[A]] = (a: List[A]) => a.toString
}

object Run extends App {

  import Show._

  val list: List[Int] = Cons(1, Cons(2, Nil))
  val otherList = List(1, 2, 3)

  println(show(list))
  println(show(Nil))
  println(show(otherList))
}
