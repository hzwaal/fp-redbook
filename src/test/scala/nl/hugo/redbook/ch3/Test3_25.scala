package nl.hugo.redbook.ch3

import org.scalatest._

class Test3_25 extends WordSpec with Matchers {

  "size" should {

    "return the number of leafs in a non-empty tree" in {
      Exercise25.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(7)
    }

    "return 1 for a tree with one element" in {
      Exercise25.size(Leaf(42)) should be(1)
    }
  }
}