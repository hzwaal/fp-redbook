package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Option => _, Some => _ }

class Test4_03 extends WordSpec with Matchers {
  "map2" should {
    "return Some(valueA) for applying a function to Some(valueB) and Some(valueA)" in {
      Option.map2(Some(1), Some(2))(_ + _) should be(Some(3))
    }

    "return None for applying a function to None and Some(value)" in {
      Option.map2(None, Some(2))((a, b) => b) == None
    }

    "return None for applying a function to Some(value) and None" in {
      Option.map2(Some(1), None)((a, b) => a) == None
    }

    "return None for applying a function to None and None" in {
      Option.map2(None, None)((a, b) => 1) == None
    }
  }
}