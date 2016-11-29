package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_12 extends WordSpec with Matchers {
  "Stream" should {
    "generate fibonacci numbers" in {
      Stream.fibsViaUnfold.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
    }
    "fromViaUnfold(10) should return incremental values" in {
      Stream.fromViaUnfold(10).take(5).toList should be(List(10, 11, 12, 13, 14))
    }

    "from(-5) should return incremental values" in {
      Stream.fromViaUnfold(-5).take(3).toList should be(List(-5, -4, -3))
    }

    "return the constant value" in {
      Stream.constantViaUnfold(5).take(10).toList should be(List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    }

    "return ones using onesViaUnfold" in {
      Stream.onesViaUnfold.take(3).toList should be(List(1, 1, 1))
    }
  }
}
