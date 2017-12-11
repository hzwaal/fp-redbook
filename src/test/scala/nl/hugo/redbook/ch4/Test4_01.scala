package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Option => _, Some => _ }

class Test4_01 extends WordSpec with Matchers {

  "map" should {
    "map Some integer to Some string" in {
      Some(1).map(_.toString) should be(Some("1"))
    }

    "map None to None" in {
      None.map(_.toString) should be(None)
    }
  }

  "getOrElse" should {
    "get option value for Some" in {
      Some(1).getOrElse(2) should be(1)
    }

    "get else value for None" in {
      None.getOrElse(2) should be(2)
    }
  }

  "flatMap" should {
    "return Some for a flatMap of Some on Some" in {
      Some(1).flatMap(_ => Some(2)) should be(Some(2))
    }

    "return None for a flatMap of None on Some" in {
      Some(1).flatMap(_ => None) should be(None)
    }

    "return None for a flatMap of Some on None" in {
      None.flatMap(_ => Some(2)) should be(None)
    }

    "return None for a flatMap of None on None" in {
      None.flatMap(_ => None) should be(None)
    }
  }

  "orElse" should {
    "return Some for Some orElse  None" in {
      Some(1).orElse(None) should be(Some(1))
    }

    "return SomeA for SomeA orElse SomeB" in {
      Some(1).orElse(Some(2)) should be(Some(1))
    }

    "return None for None orElse None" in {
      None.orElse(None) should be(None)
    }

    "return Some for None orElse Some" in {
      None.orElse(Some(1)) should be(Some(1))
    }
  }

  "filter" should {
    "return Some for Some.filter(true)" in {
      Some(1).filter(_ => true) should be(Some(1))
    }

    "return None for Some.filter(false)" in {
      Some(1).filter(_ => false) should be(None)
    }

    "return None for None.filter(true)" in {
      None.filter(_ => true) should be(None)
    }

    "return None for None.filter(false)" in {
      None.filter(_ => false) should be(None)
    }
  }
}