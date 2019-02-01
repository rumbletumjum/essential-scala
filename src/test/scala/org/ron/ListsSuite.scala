package org.ron

import org.scalatest.FunSpec

class ListsSuite extends FunSpec {
  val myList = Pair(1, Pair(2, Pair(3, End)))

  describe("An IntList") {
    describe("containing (1, 2, 3)") {
      it("should have product 6") {
        assert(myList.product == 6)
      }

      it("should have sum 6") {
        assert(myList.sum === 6)
      }
    }

    describe("its tail") {
      it("should have sum 5") {
        assert(myList.tail.sum === 5)
      }

      it("should have product 6") {
        assert(myList.tail.product === 6)
      }
    }
  }

//  "An empty IntList" should "have product == 1" in {
//    assert(End.product() === 1)
//  }
//
//  "An empty IntList" should "have sum == 0" in {
//    assert(End.sum === 0)
//  }
//
//  "An IntList containing (1, 2, 3)" should {
//    "have sum == 6" in {
//      assert(myList.sum === 6)
//    }
//  }
//
//  "IntList(1, 2, 3)" should "have product == 6" in {
//    assert(myList.sum === 6)
//  }
//
//  "IntList(1, 0, -1)" should "have sum == 0" in {
//    val zeroList = Pair(1, Pair(0, Pair(-1, End)))
//    assert(zeroList.sum === 0)
//  }
//
//  "Reverse of list(1, 2, 3)" should "be list(3, 2, 1)" in {
//    val expected = Pair(3, Pair(2, Pair(1, End)))
//    assert(reverse(myList) === expected)
//  }
}
