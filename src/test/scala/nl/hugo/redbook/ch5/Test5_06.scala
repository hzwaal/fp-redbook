package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_06 extends WordSpec with Matchers {
  "A Stream" should {
    "return its head" in {
      Stream(1, 2, 3, 4).headOption should be(Some(1))
    }

    "return its head if it has no tail" in {
      Stream(1).headOption should be(Some(1))
    }
  }

  "An Empty" should {
    "return None" in {
      Empty.headOption should be(None)
    }
  }
}
