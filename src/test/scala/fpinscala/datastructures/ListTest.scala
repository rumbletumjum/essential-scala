package fpinscala.datastructures
import fpinscala.datastructures.List._
import org.scalatest.FlatSpec

class ListTest extends FlatSpec {
  val testList = List(1, 2, 3)
  "A List containing (1, 2, 3)" should "have sum 6" in {
    assert(sum(List(1, 2, 3)) === 6)
  }

  it should "have product 6" in {
    assert(product(List(1, 2, 3)) === 6)
  }

  "A List containing 0" should "have product 0" in {
    assert(product(List(1, 2, 0, 7)) === 0)
  }

  "A List containing string (this, is, a, list)" should "have tail (is, a, list)" in {
    val list = List("this", "is", "a", "list")
    assert(tail(list) === List("is", "a", "list"))
  }

  "A one-element list" should "have tail Nil" in {
    assert(tail(List(1)) === Nil)
  }

  "A list (1, 2, 3)" should "have new head 7" in {
    assert(setHead(List(1, 2, 3), 7) === List(7, 2, 3))
  }

  "Nil" should "still be nil" in {
    assert(setHead(Nil, 7) === Nil)
  }

  assert(drop(testList, 2) === List(3))

  assert(dropWhile(testList)(x => x <= 2) === List(3))
}
