package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_04 extends WordSpec with Matchers {
  "A Stream with four elements" should {
    "return true if all elements match the predicate" in {
      Stream(1, 1, 1, 1).forAll(_ == 1) should be(true)
    }

    "return false if some elements match the predicate" in {
      Stream(1, 1, 2, 1).forAll(_ == 1) should be(false)
    }

    "return false if no elements match the predicate" in {
      Stream(2, 2, 2, 2).forAll(_ == 1) should be(false)
    }
  }

  "An Empty" should {
    "return true since no elements that mismatch the predicate" in {
      Empty.forAll(_ => false) should be(true)
    }
  }
}

