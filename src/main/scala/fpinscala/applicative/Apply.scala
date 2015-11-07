package fpinscala.applicative

import fpinscala.monad.Functor

import scala.language.higherKinds

trait Apply[F[_]] extends Functor[F] {

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap(fb)(map(fa)(f.curried))

  def tuple2[A,B](fa: => F[A], fb: => F[B]): F[(A,B)] =
    apply2(fa, fb)((_,_))

}

trait ApplyOps[F[_], A] {

  implicit val F: Apply[F]

  val self: F[A]

  def <*>[B](f: F[A => B]): F[B] =
    F.ap(self)(f)

  def tuple[B](f: F[B]): F[(A,B)] =
    F.tuple2(self,f)

  def *>[B](fb: F[B]): F[B] =
    F.apply2(self, fb)((_,b) => b)

  def <*[B](fb: F[B]): F[A] =
    F.apply2(self, fb)((a,_) => a)

  def |@|[B](fb: F[B]) = ???

}
