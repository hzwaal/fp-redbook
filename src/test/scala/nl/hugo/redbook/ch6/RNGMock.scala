package nl.hugo.redbook.ch6

case class RNGMock(value: Int, rng: RNG) extends RNG {
  def nextInt: (Int, RNG) = (value, rng)
}
