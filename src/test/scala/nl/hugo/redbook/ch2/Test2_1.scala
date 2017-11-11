package nl.hugo.redbook.ch2

import org.scalatest._

class Test2_1 extends WordSpec with Matchers {
  import Exercise01.fib

  "Fibonacci" should {

    "return 0 as first number" in {
      fib(0) should be(0)
    }

    "return 1 as second number" in {
      fib(1) should be(1)
    }

    "return sum of previous numbers" in {
      for (i <- 0 to 9) {
        fib(i) + fib(i + 1) should be(fib(i + 2))
      }
    }

    "use tail-recursive implementation" in {
      fib(10000) should be(1242044891)
    }
  }
}