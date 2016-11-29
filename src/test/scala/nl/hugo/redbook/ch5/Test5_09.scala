package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_09 extends WordSpec with Matchers {
  "Stream" should {
    "from(10) should return incremental values" in {
      Stream.from(10).take(5).toList should be(List(10, 11, 12, 13, 14))
    }

    "from(-5) should return incremental values" in {
      Stream.from(-5).take(3).toList should be(List(-5, -4, -3))
    }
  }
}
