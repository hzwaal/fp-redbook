package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Option => _, Some => _ }

class Test4_05 extends WordSpec with Matchers {
  "traverse" should {
    "return Some(List) if all items are evaluated as Some" in {
      Option.traverse(List(1, 2, 3, 4))(Some(_)) should be(Some(List(1, 2, 3, 4)))
    }

    "return None if at least one item evaluated as None" in {
      Option.traverse(List(1, 2, 3, 4))(i => if (i == 2) None else Some(i)) should be(None)
    }
  }

  "sequence_via_traverse" should {
    "transform a List(Some) into Some(List)" in {
      Option.sequence_via_traverse(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
    }

    "transform a List(Some|None) into None" in {
      Option.sequence_via_traverse(List(Some(1), None, Some(3))) should be(None)
    }

    "transform an emtpy list into Some(Nil)" in {
      Option.sequence_via_traverse(Nil) should be(Some(Nil))
    }
  }
}