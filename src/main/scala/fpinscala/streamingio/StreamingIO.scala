package fpinscala.streamingio

import java.util.concurrent.Executors

import fpinscala.io.console.ConsoleApplicationSample
import fpinscala.io.free.Free
import fpinscala.io.free.Free.IO
import fpinscala.pallarelism.nonblocking.Nonblocking.Par

import scala.io.Source

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream.empty
    case Await(receive) => s match {
      case x #:: xs => receive(Some(x))(xs)
      case xs => receive(None)(xs)
    }
    case Emit(x, xs) => x #:: xs(s)

  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(receive) => Await {
        case None => receive(None)
        case i => go(receive(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Halt() => Halt() |> f(None)
      case Emit(h, t) => t |> f(Some(h))
      case Await(g) => Await(i => g(i) |> p2)
    }
  }

  def map[O2](f: O => O2): Process[I, O2] =
    this |> Process.lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(f) => Await(f.andThen(_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(g) => Await(g.andThen(_.flatMap(f)))
  }

  def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] =
    Process.zip(this, p)

  def zipWithIndex: Process[I, (O, Int)] =
    zip(Process.count.map(_ - 1))

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](receive: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case None => Halt()
    case Some(i) => Emit(f(i))
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await { (i: I) => f(i, z) match {
      case (o, s2) => Emit(o, loop(s2)(f))
    }
  }

  def await[I, O](f: I => Process[I, O],
                  fallback: Process[I, O] = Halt[I, O]()) = Await[I, O] {
    case Some(i) => f(i)
    case None => fallback
  }

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
    (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) =>
        Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) =>
        Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

  def feed[A,B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
    p match {
      case Halt() => p
      case Emit(h,t) => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }

  def any: Process[Boolean, Boolean] =
    loop(false)((b: Boolean, s) => (s || b, s || b))

  def count[I]: Process[I,Int] =
    lift((i: I) => 1.0) |> sum |> lift(_.toInt)

  def sum: Process[Double, Double] =
    loop(0.0)((d: Double, acc) => (acc + d, acc+  d))

  def exists[I](f: I => Boolean): Process[I, Boolean] =
    lift(f) |> any

}

object NonComposableProcessingWithIO {

  def lineGt40k_1(filename: String): IO[Boolean] = IO {
    val src = Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    } finally src.close()
  }

  def lineGt40k_2(filename: String): IO[Boolean] =
    lines(filename).map(_.zipWithIndex.exists(_._2 + 1 >= 40000))


  def lines(filename: String): IO[Stream[String]] = IO {
    val src = Source.fromFile(filename)
    src.getLines().toStream.append {
      src.close()
      Stream.empty
    }
  }

  def runLineGt40k: Boolean = {
    val app: IO[Boolean] = NonComposableProcessingWithIO.lineGt40k_2("/Users/ada-u/.tmux.conf")
    Par.run(Executors.newCachedThreadPool)(Free.run(app)(ConsoleApplicationSample.par0Monad))
  }

}
