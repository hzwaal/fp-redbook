package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_11 extends WordSpec with Matchers {
  "Stream" should {
    "implement a from(7) using unfold" in {
      Stream.unfold(7)(x => Some(x, x + 1)).take(4).toList should be(List(7, 8, 9, 10))
    }
  }
}
