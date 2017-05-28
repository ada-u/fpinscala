package fpinscala.applicative

import fpinscala.collection.list.MyList
import fpinscala.collection.stream.MyStream
import fpinscala.monad.{Functor, Monad}
import fpinscala.monoid.Monoid
import fpinscala.option.MyOption
import fpinscala.state.State

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {

  // primitives
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  //map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  // derivatives
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit())((a, _) => f(a))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit[(A) => (B) => (C) => D](f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit[(A) => (B) => (C) => (D) => E](f.curried))(fa))(fb))(fc))(fd)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def replicateM[A](n: Int, fa: F[A]): F[MyList[A]] =
    sequence(MyList.fill(n)(fa))

  def sequence[A](fas: MyList[F[A]]): F[MyList[A]] =
    traverse(fas)(fa => fa)

  def traverse[A, B](as: MyList[A])(f: A => F[B]): F[MyList[B]] =
    as.foldRight(unit(MyList.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))
/*
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)
  
  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(0)._1

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as: List[A] <- State.get[List[A]]
      dummy: Unit <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse
*/
}

object Applicative {

  type Const[M, B] = M

  val optionApplicative: Applicative[MyOption] = new Applicative[MyOption] {

    override def map2[A, B, C](fa: MyOption[A], fb: MyOption[B])(f: (A, B) => C): MyOption[C] =
      for (a <- fa; b <- fb) yield f(a, b)

    override def unit[A](a: => A): MyOption[A] =
      MyOption(a)

  }

  val streamApplicative: Applicative[MyStream] = new Applicative[MyStream] {

    override def map2[A, B, C](fa: MyStream[A], fb: MyStream[B])(f: (A, B) => C): MyStream[C] =
      fa.zip(fb).map(f.tupled)

    override def unit[A](a: => A): MyStream[A] =
      MyStream.constant(a)
  }

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M =
        M.zero

      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M =
        M.append(m1, m2)
    }

}
