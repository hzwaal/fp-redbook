package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_15 extends WordSpec with Matchers {
  "Stream" should {
    "produce tails" in {
      Stream(1, 2, 3).tails.map(_.toList).toList should be(List(List(1, 2, 3), List(2, 3), List(3), Nil))
    }
  }

  "An Empty" should {
    "produce no tails" in {
      Empty.tails.toList should be(List(Empty))
    }
  }
}

