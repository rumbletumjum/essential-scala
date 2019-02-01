package io.github.rumbletumjum.essentialscala

import scala.annotation.tailrec

sealed trait IntList {

  def length: Int =
    this match {
      case End         => 0
      case Pair(_, tl) => 1 + tl.length
    }

  def double: IntList =
    this match {
      case End          => End
      case Pair(hd, tl) => Pair(hd * 2, tl.double)
    }

  def sum: Int =
    this match {
      case End          => 0
      case Pair(hd, tl) => hd + tl.sum
    }

  def product: Int = {
    @tailrec
    def product(list: IntList, acc: Int = 1): Int =
      list match {
        case End          => acc * 1
        case Pair(hd, tl) => product(tl, hd * acc)
      }
    product(this)
  }

  def last: Option[Int] =
    this match {
      case Pair(hd, End) => Some(hd)
      case Pair(_, tl)   => tl.last
      case End           => None
    }

  // To see if defining Haskell style operators works -- it does!
  def >>=(int: Int): IntList = Pair(int, this)
}

case object End extends IntList
case class Pair(head: Int, tail: IntList) extends IntList

object IntListFns {

  def recLength(list: IntList, total: Int = 0): Int = {
    list match {
      case End         => total
      case Pair(_, tl) => recLength(tl, total + 1)
    }
  }

  def reverse(list: IntList): IntList = {
    def reverse(list: IntList, acc: IntList): IntList = {
      list match {
        case End          => acc
        case Pair(hd, tl) => reverse(tl, acc >>= hd)
      }
    }
    reverse(list, End)
  }

  def double(list: IntList): IntList = {
    list match {
      case End          => End
      case Pair(hd, tl) => Pair(hd * 2, double(tl))
    }
  }
}
