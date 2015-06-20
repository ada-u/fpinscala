package fpinscala.validation

import fpinscala.applicative.Applicative

sealed trait Validation[+E, +A]

case class Success[A](a: A) extends Validation[Nothing, A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

object Validation {

  implicit def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x]})#f] = new Applicative[({ type f[x] = Validation[E, x]})#f] {

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a1), Success(a2)) => Success(f(a1, a2))
      case (Success(_), e@Failure(_, _)) => e
      case (e@Failure(_, _), Success(_)) => e
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
    }

    override def unit[A](a: => A): Validation[E, A] =
      Success(a)
  }

}
