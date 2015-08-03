package fpinscala.io.console

import fpinscala.io.free.Free.~>
import fpinscala.io.free.{Suspend, Free}
import fpinscala.pallarelism.nonblocking.Nonblocking.Par

import scala.io.StdIn._
import scala.util.Try

sealed trait Console[A] {

  def toPar: Par[A]

  def toThunk: () => A

}

object Console {

  type ConsoleIO[A] = Free[Console, A]

  val consoleToFunction0: (Console ~> Function0) = new (Console ~> Function0) {
    def apply[A](a: Console[A]): () => A = a.toThunk
  }

  val consoleToPar: (Console ~> Par) = new (Console ~> Par) {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

}

case object ReadLine extends Console[Option[String]] {

  def toPar: Par[Option[String]] = Par.lazyUnit(run)

  def toThunk: () => Option[String] = () => run

  def run: Option[String] = Try(readLine()).toOption

}

case class PrintLine(line: String) extends Console[Unit] {

  def toPar: Par[Unit] = Par.lazyUnit(println(line))

  def toThunk: () => Unit = () => println(line)

}
