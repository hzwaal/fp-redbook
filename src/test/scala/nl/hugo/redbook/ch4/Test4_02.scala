package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Option => _, Some => _ }

class Test4_02 extends WordSpec with Matchers {
  "variance" should {
    "return Some(variance) for a list of values" in {
      Option.variance(List(1.0, -1.0)) should be(Some(1.0))
    }

    "return Some(variance) of a list of a single value" in {
      Option.variance(List(1.0)) should be(Some(0.0))
    }

    "return None for an empty list" in {
      Option.variance(Nil) should be(None)
    }
  }
}