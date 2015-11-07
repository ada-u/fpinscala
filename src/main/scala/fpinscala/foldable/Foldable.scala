package fpinscala.foldable

import fpinscala.collection.list.MyList
import fpinscala.monoid.Monoid

import scala.language.higherKinds

trait Foldable[F[_]] {

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(Monoid.dual(Monoid.endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.append(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.append)

  def toList[A](as: F[A]): MyList[A] =
    foldRight(as)(MyList[A]())(_ :: _)

}
