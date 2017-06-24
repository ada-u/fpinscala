package fpinscala.streamingio

import java.io.FileWriter

import fpinscala.io.free.Free.IO
import fpinscala.monad.MonadCatch

import scala.language.{higherKinds, postfixOps}

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

  @annotation.tailrec
  final def kill[O2]: Process[F,O2] = this match {
    case Await(_, recv) => recv(Left(Kill)).drain.onHalt {
      case Kill => Halt(End) // we convert the `Kill` exception back to normal termination
      case e => Halt(e)
    }
    case Halt(e) => Halt(e)
    case Emit(_, t) => t.kill
  }

  def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F,O3] = {
    t match {
      case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h, t) => Emit(h, (this tee p2)(t))
      case Await(side, recv) => side.get match {
        case Left(isO) => this match {
          case Halt(e) => p2.kill onComplete Halt(e)
          case Emit(o,ot) => (ot tee p2)(Try(recv(Right(o))))
          case Await(reqL, recvL) =>
            await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
        }
        case Right(isO2) => p2 match {
          case Halt(e) => this.kill onComplete Halt(e)
          case Emit(o2, ot) => (this tee ot)(Try(recv(Right(o2))))
          case Await(reqR, recvR) =>
            await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
        }
      }
    }
  }

  def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
    (this tee p2)(Process.zipWith(f))

  def to[O2](sink: Sink[F, O]): Process[F, Unit] =
    join[F, Unit] {
      // Process[F, Process[F, Unit]]
      (this zipWith sink)((o: O, f: O => Process[F, Unit]) => f(o))
    }

  def through[O2](channel: Channel[F, O, O2]): Process[F, O2] =
    join[F, O2] {
      // Process[F, Process[F, O2]]
      (this zipWith channel)((o: O, f: O => Process[F, O2]) => f(o))
    }

  def |>[O2](p2: Process1[O,O2]): Process[F,O2] = {
    p2 match {
      case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
      case Emit(h, t) => Emit(h, this |> t)
      case Await(req,recv) => this match {
        case Halt(err) => Halt(err) |> recv(Left(err))
        case Emit(h,t) => t |> Try(recv(Right(h)))
        case Await(req0,recv0) => await(req0)(recv0 andThen (_ |> p2))
      }
    }
  }

  def take(n: Int): Process[F,O] =
    this |> Process.take(n)

  def once: Process[F,O] =
    take(1)

  def pipe[O2](p2: Process1[O,O2]): Process[F,O2] =
    this |> p2

  def filter(f: O => Boolean): Process[F,O] =
    this |> Process.filter(f)

  def map[O2](f: O => O2): Process[F,O2] = this match {
    case Await(req,recv) =>
      Await(req, recv andThen (_ map f))
    case Emit(h, t) => Try { Emit(f(h), t map f) }
    case Halt(err) => Halt(err)
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

  type Channel[F[_],I,O] = Process[F, I => Process[F, O]]

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

  def fileW(file: String, append: Boolean = false): Sink[IO,String] =
    resource[FileWriter, String => Process[IO, Unit]]
      { IO { new FileWriter(file, append) } }
      { (w: FileWriter) =>
        constant {
          (s: String) => eval[IO, Unit](IO(w.write(s)))
        }
      }
      { w => eval_(IO(w.close())) }

  def haltT[I,I2,O]: Tee[I,I2,O] =
    Halt[T[I,I2]#f,O](End)

  def awaitL[I,I2,O](recv: I => Tee[I,I2,O],
                     fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    await[T[I,I2]#f,I,O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def awaitR[I,I2,O](recv: I2 => Tee[I,I2,O],
                     fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    await[T[I,I2]#f,I2,O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
    emit(h, tl)

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
    awaitL[I, I2, O](i  =>
      awaitR(i2 => emitT(f(i, i2)))) repeat

  def constant[A](a: A): Process[IO, A] =
    eval[IO, A](IO(a)).repeat

  def join[F[_],A](p: Process[F, Process[F, A]]): Process[F, A] =
    p.flatMap(pa => pa)

  def id[I]: Process1[I, I] =
    await1[I, I]((i: I) => emit(i, id))

  def intersperse[I](sep: I): Process1[I, I] =
    await1[I, I] { i: I =>
      emit1[I, I](i) ++ id.flatMap[I] { i: I =>
        emit1[I, I](sep) ++ emit1[I, I](i)
      }
    }

  def take[I](n: Int): Process1[I,I] =
    if (n <= 0) halt1
    else await1[I, I](i => emit(i, take(n-1)))

}

