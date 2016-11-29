package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_16 extends WordSpec with Matchers {
  "Stream" should {
    "scanright over the stream and add all the numbers in the tails" in {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
    }
  }
}
