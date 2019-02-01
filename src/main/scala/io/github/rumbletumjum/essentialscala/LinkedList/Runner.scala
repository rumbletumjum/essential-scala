package io.github.rumbletumjum.essentialscala.LinkedList

object Runner {

  def main(args: Array[String]): Unit = {
    val linkedList = LinkedList(1, 2, 3)

    assert(linkedList(1) == 2)

    println("-- Assert list length is 1")
    assert(linkedList.length == 3)

    assert(linkedList.contains(2))
    assert(!linkedList.contains(7))
  }
}

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]
