package fpinscala.io.free

import java.util.concurrent.ExecutorService

import fpinscala.io.console.Console
import fpinscala.monad.Monad
import fpinscala.pallarelism.nonblocking.Nonblocking.Par

import scala.language.higherKinds

sealed trait Free[F[_], A] {

  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]
  type IO[A] = Free[Par, A]
  type ~>[F[_], G[_]] = Translate[F, G]

  def IO[A](a: => A): IO[A] = Suspend[Par, A](Par.delay(a))

  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A =
    Par.run(E) {
      run(io)(Monad.nbParMonad)
    }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FG[A] = Free[G, A]
    // Free[Function0, A]の中身にアクセスするには、runFreeをする必要がある
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

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(new (Console ~> Function0) {
      def apply[A](f: Console[A]): () => A = f.toThunk
    }))

  trait Translate[F[_], G[_]] {

    def apply[A](f: F[A]): G[A]

  }

}

trait Files[A]

trait HandleR

trait HandleW

case class OpenRead(file: String) extends Files[HandleR]

case class OpenWrite(file: String) extends Files[HandleW]

case class ReadLine(h: HandleR) extends Files[Option[String]]

case class WriteLine(h: HandleW, line: String) extends Files[Unit]

object Files {

  import Free._

  val filesToPar: (Files ~> Par) = new (Files ~> Par) {
    def apply[A](f: Files[A]): Par[A] = ???
  }
  val lines = List(71, 82, 73, 74, 76, 78, 79, 80)
  val cs = movingAvg(3)(lines.map(s => fahrenheitToCelsius(s.toDouble))).map(_.toString)

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def loop(f: HandleR, c: HandleW): Free[Files, Unit] =
    for {
      line <- Suspend(ReadLine(f))
      _ <- line match {
        case None => Return[Files, Unit](())
        case Some(s) =>
          Suspend[Files, Unit] {
            WriteLine(c, fahrenheitToCelsius(s.toDouble).toString)
          } flatMap (_ => loop(f, c))

       //   FlatMap[Files, Unit, Unit](Suspend[Files, Unit](WriteLine(c, fahrenheitToCelsius(s.toDouble).toString)), (_) => loop(f, c))
      }
    } yield ()

  def convertFiles = for {
    f <- Suspend(OpenRead("fahrenheit.txt"))
    c <- Suspend(OpenWrite("celsius.txt"))
    _ <- loop(f, c)
  } yield ()

  def movingAvg(n: Int)(l: List[Double]): List[Double] = ???

}
