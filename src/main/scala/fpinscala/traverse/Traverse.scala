package fpinscala.traverse

import fpinscala.applicative.Applicative
import fpinscala.foldable.Foldable
import fpinscala.monad.{Monad, Functor}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fga: F[G[A]])( implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    val reuslt: Id[F[B]] = traverse[Id, A, B](fa)(f)(idMonad)

  }
}