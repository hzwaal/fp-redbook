package nl.hugo.redbook.ch4

import scala.{ Either => _, Option => _, Some => _ }

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Exercise 4.01
  def map[B](f: A => B): Option[B] = ???

  // Exercise 4.01
  def getOrElse[B >: A](default: => B): B = ???

  // Exercise 4.01
  def flatMap[B](f: A => Option[B]): Option[B] = ???

  // Exercise 4.01
  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  // Exercise 4.01
  def filter(f: A => Boolean): Option[A] = ???
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.02
  def variance(xs: Seq[Double]): Option[Double] = ???

  // Exercise 4.03
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  // Exercise 4.04
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  // Exercise 4.05
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // Exercise 4.05
  def sequence_via_traverse[A](a: List[Option[A]]): Option[List[A]] = ???

}