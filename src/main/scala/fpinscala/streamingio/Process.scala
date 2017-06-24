package fpinscala.streamingio

import fpinscala.io.free.Free.IO
import fpinscala.monad.MonadCatch

import scala.language.higherKinds

trait Process[F[_], O] {

  import Process._

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(request, receive) => Await(request, receive.andThen(_.onHalt(f)))
  }

  def onComplete(p: => Process[F, O]): Process[F, O] = onHalt {
    case End => p.asFinalizer
    case error => p.asFinalizer ++ Halt(error)
  }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, receive) => await(req) {
      case Left(Kill) => asFinalizer
      case x => receive(x)
    }
  }

  def ++(p: => Process[F, O]): Process[F, O] = onHalt {
    case End => Try(p)
    case error => Halt(error)
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(error) => Halt(error)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(request, receive) => await(request)(receive.andThen(_.flatMap(f)))
  }

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(error) => F.fail(error)
        case Await(request, receive) => F.flatMap(F.attempt(request)) {
          e => go(Try(receive(e)), acc)
        }
      }

    go(this, IndexedSeq())
  }

  final def drain[O2]: Process[F, O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(h, t) => t.drain
    case Await(request, receive) => Await(request, receive.andThen(_.drain))
  }

  def repeat: Process[F,O] =
    this ++ this.repeat
}

case object End extends Exception

case object Kill extends Exception

object Process {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case class Is[I]() {

    val Get = new f[I] {}

    sealed trait f[X]

  }

  case class T[I,I2]() {
    sealed trait f[X] {
      def get: Either[I => X, I2 => X]
    }
    val L = new f[I] {
      def get = Left(identity)
    }
    val R = new f[I2] {
      def get = Right(identity)
    }
  }

  def L[I,I2] = T[I,I2]().L

  def R[I,I2] = T[I,I2]().R

  type Process1[I, O] = Process[Is[I]#f, O]

  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  type Tee[I, I2, O] = Process[T[I, I2]#f, O]

  def emit1[I, O](h: O, tl: Process1[I, O] = halt1[I, O]): Process1[I, O] = emit(h, tl)

  def await[F[_], A, O](request: F[A])
                       (receive: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await[F, A, O](request, receive)

  def lift[I, O](f: I => O): Process1[I, O] =
    await1[I, O](i => emit(f(i))) repeat

  def emit[F[_], O](
                     head: O,
                     tail: Process[F, O] = Halt[F, O](End)): Process[F, O] =
    Emit(head, tail)

  def await1[I, O](receive: I => Process1[I, O],
                   fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(error) => Halt(error)
      case Right(i) => Try(receive(i))
    })

  def Get[I] = Is[I]().Get

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p catch {
      case e: Throwable => Halt(e)
    }

  def filter[I](f: I => Boolean): Process1[I, I] =
    await1[I, I](i => if (f(i)) emit(i) else halt1) repeat

  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

  def eval[F[_], A](a: F[A]): Process[F, A] =
    await[F, A, A](a) {
      case Left(err) => Halt(err)
      case Right(a2) => Emit(a2, Halt(End))
    }

  def eval_[F[_], A, B](a: F[A]): Process[F, B] =
    eval[F, A](a).drain[B]

  def resource[R, O](acquire: IO[R])
                    (use: R => Process[IO, O])
                    (release: R => Process[IO, O]): Process[IO, O] =
    eval[IO, R](acquire).flatMap { r =>
      use(r).onComplete(release(r))
    }

  def resource_[R, O](acquire: IO[R])
                     (use: R => Process[IO, O])
                     (release: R => IO[Unit]): Process[IO, O] =
    resource(acquire)(use)(release.andThen(eval_[IO, Unit, O]))

  def lines(filename: String): Process[IO, String] =
    resource {
      IO(io.Source.fromFile(filename))
    } { src =>
      lazy val iterator = src.getLines()

      def step = if (iterator.hasNext) Some(iterator.next()) else None

      lazy val lines: Process[IO, String] = eval[IO, Option[String]](IO(step)).flatMap {
        case None => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    } { src =>
      eval_[IO, Unit, String](IO(src.close()))
    }


}

