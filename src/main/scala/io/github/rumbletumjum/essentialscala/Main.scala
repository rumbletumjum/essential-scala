package io.github.rumbletumjum.essentialscala
import io.github.rumbletumjum.essentialscala.IntTree.{Leaf, Node, Tree}
import org.ron.Show.show

object Main {
  def main(args: Array[String]): Unit = {
    val myList = Pair(1, Pair(2, Pair(3, End)))

    assert(myList.double == Pair(2, Pair(4, Pair(6, End))))

    println(myList.double)

    println(myList.product)
    assert(myList.product == 6)
    assert(myList.tail.product == 6)

    val tree = Node(Leaf(1), Leaf(7))

    assert(Tree.sum(tree) == 8)
    assert(tree.sum == 8)

    assert(myList.last.contains(3))

    myList.last foreach {
      println
    }

    println(show(8))


//    val numbers: List[Int] = 2 :: 3 :: 3 :: Nil
//    val oneMore: List[Int] = 4 :: numbers

    val numbers = Range(1, 20).toList
    val add2 = curry(add)(1)

    val strings = numbers map { add2 } map { _.toString }

    val withComma = formatListFn(", ")

    println(withComma(numbers))
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def formatList(sep: String)(list: List[Any]) = list mkString sep

  val formatListFn = (sep: String) => (list: List[Any]) => list mkString sep


  def add(a: Int, b: Int): Int = a + b

  val addFn = (a: Int, b: Int) => a + b

  val addFn2 = (a: Int) => (b: Int) => a + b

}
