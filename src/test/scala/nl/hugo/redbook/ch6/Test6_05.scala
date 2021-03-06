package nl.hugo.redbook.ch6

import nl.hugo.redbook.Spec

class Test6_05 extends Spec {

  implicit val last = CNG.zero

  "double (via map)" should {

    "return random doubles between 0 and 1" in {
      var rng: RNG = RNG.Simple(System.currentTimeMillis)
      for (_ <- 1 to 1000) {
        val (d, r) = RNG.doubleViaMap(rng)
        rng = r
        d should be >= 0.0
        d should be < 1.0
      }
    }

    "return a new generator" in {
      val rng = CNG(0)
      val (_, r) = RNG.doubleViaMap(rng)
      r should be(last)
    }

    "handle lower corner case" in {
      val rng = CNG(0)
      val (d, _) = RNG.doubleViaMap(rng)
      d should be >= 0.0
      d should be < 1.0
    }

    "handle upper corner case" in {
      val rng = CNG(Int.MaxValue)
      val (d, _) = RNG.doubleViaMap(rng)
      d should be >= 0.0
      d should be < 1.0
    }
  }
}