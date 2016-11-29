package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_01 extends WordSpec with Matchers {
  "A Stream" should {
    "return a list with its contents" in {
      Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
    }

    "return an empty list if it is empty" in {
      Empty.toList should be(Nil)
    }
  }
}
