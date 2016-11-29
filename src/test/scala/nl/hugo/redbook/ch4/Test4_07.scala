package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Left => _, Option => _, Right => _ }

class Test4_07 extends WordSpec with Matchers {
  "sequence" should {
    "return the first Left from a list of Lefts" in {
      Either.sequence(List(Left(0), Left(1), Left(2))) should be(Left(0))
    }

    "return the first Left from a list of Lefts and Rights" in {
      Either.sequence(List(Right(0), Right(1), Left(2))) should be(Left(2))
    }

    "return a Right(List) from a list of Rights" in {
      Either.sequence(List(Right(0), Right(1), Right(2))) should be(Right(List(0, 1, 2)))
    }
  }

  "traverse" should {
    "return the first Left from a list of Lefts" in {
      Either.traverse(List(Left(0), Left(1), Left(2)))(_.map(_.toString)) should be(Left(0))
    }

    "return the first Left from a list of Lefts and Rights" in {
      Either.traverse(List(Right(0), Right(1), Left(2)))(_.map(_.toString)) should be(Left(2))
    }

    "return a Right(List) with the function applied from a list of Rights" in {
      Either.traverse(List(Right(0), Right(1), Right(2)))(_.map(_.toString)) should be(Right(List("0", "1", "2")))
    }
  }
}