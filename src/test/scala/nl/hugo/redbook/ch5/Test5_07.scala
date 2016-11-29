package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_07 extends WordSpec with Matchers {
  "A Stream" should {
    "return its mapped values" in {
      Stream(1, 2, 3, 4).map(_.toString).toList should be(List("1", "2", "3", "4"))
    }

    "filter items that match the predicate" in {
      Stream(3, 1, 2, 4).filter(_ < 3).toList should be(List(1, 2))
    }

    "return Empty if no items match the predicate" in {
      Stream(1, 2, 3, 4).filter(_ => false).toList should be(Nil)
    }

    "append with another stream" in {
      Stream(1, 2).append(Stream(3, 4)).toList should be(List(1, 2, 3, 4))
    }

    "return itself when appended with an Empty" in {
      Stream(1, 2).append(Empty).toList should be(List(1, 2))
    }

    "should apply a function via flatMap" in {
      Stream(1, 2).flatMap(x => Stream(x, x)).toList should be(List(1, 1, 2, 2))
    }
  }

  "An Empty" should {
    "return Empty when mapped" in {
      Empty.map(_.toString) should be(Empty)
    }

    "return Empty when filtered" in {
      Empty.filter(_ => true) should be(Empty)
    }

    "return a Stream when it's appended" in {
      Empty.append(Stream(3, 4)).toList should be(List(3, 4))
    }

    "return Empty when appended with Empty" in {
      Empty.append(Empty) should be(Empty)
    }

    "return Empty when flatMapped" in {
      Empty.flatMap(_ => Stream(1)) should be(Empty)
    }
  }
}
