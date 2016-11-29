package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_14 extends WordSpec with Matchers {
  "Stream" should {
    "start with" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
    }

    "should not start with" in {
      Stream(1, 2, 3).startsWith(Stream(1, 3)) should be(false)
    }
  }
}


