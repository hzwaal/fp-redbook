package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

import scala.{ Either => _, Left => _, Option => _, Right => _ }

class Test4_06 extends WordSpec with Matchers {
  "map" should {
    "return Left" in {
      Left(1).map(_.toString) should be(Left(1))
    }

    "return the mapped Right" in {
      Right(2).map(_.toString) should be(Right("2"))
    }
  }

  "flatMap" should {
    "return Left when flatMapped with a Right" in {
      Left(1).flatMap(_ => Right(2)) should be(Left(1))
    }

    "return Left(A) when flatMapped with a Left(B)" in {
      Left(1).flatMap(_ => Left(2)) should be(Left(1))
    }

    "return Right(B) when applied to Right(A)" in {
      Right(1).flatMap(_ => Right(2)) should be(Right(2))
    }

    "return Right when applied to Left" in {
      Left(1).flatMap(_ => Right(2)) should be(Right(2))
    }
  }

  "orElse" should {
    "return Right when applied to Left" in {
      Left(1).orElse(Right(2)) should be(Right(2))
    }

    "return LeftB when applied to LeftA" in {
      Left(1).orElse(Left(2)) should be(Left(2))
    }

    "return Right when Left is applied to Right" in {
      Right(1).orElse(Left(2)) should be(Right(1))
    }

    "return RightA when RightB is applied to RightA" in {
      Right(1).orElse(Right(2)) should be(Right(1))
    }
  }

  "map2" should {
    def f(a: Int, b: Double) = (a + b).toString

    "return LeftA when map2 with LeftB" in {
      Left(1).map2(Left(2))(f) should be(Left(1))
    }

    "return Left when map2 with Right" in {
      Left(1).map2(Right(1.0))(f) should be(Left(1))
    }

    "return Left when Right map2 Left" in {
      Right(100).map2(Left(1))(f) should be(Left(1))
    }

    "return Right(f(A,B) when RightA map2 RightB" in {
      Right(100).map2(Right(1.0))(f) should be(Right("101.0"))
    }
  }
}