package fpinscala.traverse

import fpinscala.applicative.Applicative
import fpinscala.applicative.Applicative._
import fpinscala.foldable.Foldable
import fpinscala.monad.{Functor, Monad}
import fpinscala.monoid.Monoid

import scala.language.higherKinds

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)


  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))
}

object Traverse {

  val listTraverse = new Traverse[List] {

    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))

  }

//  val treeTraverse = new Traverse[] {}
}