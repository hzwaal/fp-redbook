package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Option => _, Some => _ }

class Test4_04 extends WordSpec with Matchers {
  "sequence" should {
    "transform a List(Some) into Some(List)" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
    }

    "transform a List(Some|None) into None" in {
      Option.sequence(List(Some(1), None, Some(3))) should be(None)
    }

    "transform an emtpy list into Some(Nil)" in {
      Option.sequence(Nil) should be(Some(Nil))
    }
  }
}