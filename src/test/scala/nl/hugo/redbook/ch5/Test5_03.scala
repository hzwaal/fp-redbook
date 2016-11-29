package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_03 extends WordSpec with Matchers {
  "A Stream with four elements" should {
    "take the elements matching the predicate" in {
      Stream(1, 1, 2, 2).takeWhile(_ == 1).toList should be(List(1, 1))
    }

    "return an empty list when no elements match the predicate" in {
      Stream(2, 2, 2, 2).takeWhile(_ == 1).toList should be(Nil)
    }

    "return all elements if they all match the predicate" in {
      Stream(1, 1, 1, 1).takeWhile(_ == 1).toList should be(List(1, 1, 1, 1))
    }

    "return all elements upto mismatching element, not the other matching elements" in {
      Stream(1, 1, 2, 1).takeWhile(_ == 1).toList should be(List(1, 1))
    }
  }

  "An Empty" should {
    "return an Empty" in {
      Empty.takeWhile(_ => true) should be(Empty)
    }
  }
}

