package fpinscala.io.free

import fpinscala.monad.Monad
import fpinscala.pallarelism.nonblocking.Nonblocking.Par
import java.util.concurrent.ExecutorService

import scala.collection.mutable

sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]
  type IO[A] = Free[Par, A]

  def IO[A](a: => A): IO[A] = Suspend[Par, A](Par.delay(a))

  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A =
    Par.run(E) {
      run(io)(Monad.nbParMonad)
    }

  trait Translate[F[_], G[_]] {

    def apply[A](f: F[A]): G[A]

  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FG[A] = Free[G, A]
    runFree(f)(new (F ~> FG) {
      def apply[A](fa: F[A]): FG[A] = Suspend(fg(fa))
    })(freeMonad[G])
  }

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {

    def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      ma.flatMap(f)

    def unit[A](a: => A): Free[F, A] =
      Return(a)

  }

  def runTrampoline[A](fa: Free[Function0, A]): A = fa match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y.flatMap {
        y => g(y).flatMap(f)
      })
    }
  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }

  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(y => f(y).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

}
