package io.github.rumbletumjum.essentialscala.IntTree

sealed trait Tree {
  def sum: Int
}

object Tree {
  def sum(tree: Tree): Int = {
    tree match {
      case Node(left, right) => sum(left) + sum(right)
      case Leaf(e) => e
    }
  }
}

case class Node(left: Tree, right: Tree) extends Tree {
  def sum: Int = left.sum + right.sum
}
case class Leaf(e: Int) extends Tree {
  def sum: Int = e
}
