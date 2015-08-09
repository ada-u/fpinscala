package fpinscala.io.console

import fpinscala.io.free.{Suspend, Free}
import fpinscala.monad.Monad
import fpinscala.pallarelism.nonblocking.Nonblocking.Par

import scala.io.StdIn._
import scala.util.Try

sealed trait Console[A] {

  def toPar: Par[A]

  def toThunk: () => A

  def toReader: ConsoleReader[A]

  def toState: ConsoleState[A]

}

object Console {

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

}

case object ReadLine extends Console[Option[String]] {

  def toPar: Par[Option[String]] = Par.lazyUnit(run)

  def toThunk: () => Option[String] = () => run

  def run: Option[String] = Try(readLine()).toOption

  def toReader: ConsoleReader[Option[String]] = ???

  def toState: ConsoleState[Option[String]] = ???
}

case class PrintLine(line: String) extends Console[Unit] {

  def toPar: Par[Unit] = Par.lazyUnit(println(line))

  def toThunk: () => Unit = () => println(line)

  def toReader: ConsoleReader[Unit] = ???

  def toState: ConsoleState[Unit] = ???
}

case class ConsoleReader[A](run: String => A) {

  def map[B](f: A => B): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)))

  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)).run(r))

}

object ConsoleReader {

  implicit val consoleReaderMonad: Monad[ConsoleReader] = new Monad[ConsoleReader] {

    def flatMap[A, B](ma: ConsoleReader[A])(f: (A) => ConsoleReader[B]): ConsoleReader[B] =
      ma.flatMap(f)

    def unit[A](a: => A): ConsoleReader[A] =
      ConsoleReader(_ => a)
  }

}

case class Buffers(in: List[String], out: Vector[String])

case class ConsoleState[A](run: Buffers => (A, Buffers)) {

  def map[B](f: A => B): ConsoleState[B] = ConsoleState { r =>
    val (a, s) = run(r)
    (f(a), s)
  }

  def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState { r =>
    val (a, s) = run(r)
    f(a).run(s)
  }

}

object ConsoleState {

  implicit val consoleStateMonad: Monad[ConsoleState] = new Monad[ConsoleState] {

    def flatMap[A, B](ma: ConsoleState[A])(f: (A) => ConsoleState[B]): ConsoleState[B] =
      ma.flatMap(f)

    def unit[A](a: => A): ConsoleState[A] =
      ConsoleState(buf => (a, buf))
  }

}