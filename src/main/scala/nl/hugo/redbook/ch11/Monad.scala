package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch7.Par._
import nl.hugo.redbook.ch8.Gen
import nl.hugo.redbook.ch9.Parsers

import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise 11.3
  def sequence[A](lma: List[M[A]]): M[List[A]] = ???

  // Exercise 11.3
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = ???

  // Exercise 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = ???

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ???

  // Exercise 11.7
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Exercise 11.8
  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] = ???

  // Exercise 11.13
  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = ???

  // Exercise 11.1
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  // Exercise 11.1
  val optionMonad: Monad[Option] = ???

  // Exercise 11.1
  val streamMonad: Monad[Stream] = ???

  // Exercise 11.1
  val listMonad: Monad[List] = ???

  // Exercise 11.2
  def stateMonad[S] = ???

  // Exercise 11.17
  val idMonad: Monad[Id] = ???

  // Exercise 11.20
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

case class Id[A](value: A) {
  // Exercise 11.17
  def map[B](f: A => B): Id[B] = ???
  // Exercise 11.17
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}
