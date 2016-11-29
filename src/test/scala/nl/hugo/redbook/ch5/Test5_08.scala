package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_08 extends WordSpec with Matchers {
  "Stream.constant" should {
    "return the constant value" in {
      Stream.constant(5).take(10).toList should be(List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    }
  }
}
