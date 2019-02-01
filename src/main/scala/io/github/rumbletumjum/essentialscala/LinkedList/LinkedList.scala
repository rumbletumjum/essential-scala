package io.github.rumbletumjum.essentialscala.LinkedList

sealed trait LinkedList[+A] {

  def length: Int =
    this match {
      case End         => 0
      case Pair(_, tl) => 1 + tl.length
    }

  def contains[B >: A](b: B): Boolean =
    this match {
      case End          => false
      case Pair(hd, tl) => if (hd == b) true else tl.contains(b)
    }

  def apply[B >: A](index: Int): B =
    this match {
      case End          => throw new IndexOutOfBoundsException
      case Pair(hd, tl) => if (index == 0) hd else tl(index - 1)
    }
//  def fold(end: Int, f: (Int, Int) => Int): Int = ???
}
object LinkedList {

  def apply[A](xs: A*): LinkedList[A] = {
    if (xs.isEmpty) End
    else Pair(xs.head, apply(xs.tail: _*))
  }
}

case object End extends LinkedList[Nothing]
final case class Pair[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]