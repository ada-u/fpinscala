package fpinscala.io.console

import fpinscala.io.free.Free
import fpinscala.io.free.Free._
import fpinscala.monad.Monad
import fpinscala.pallarelism.nonblocking.Nonblocking._

object ConsoleApplicationSample {

  val consoleToFunction0: (Console ~> Function0) = new (Console ~> Function0) {
    def apply[A](a: Console[A]): () => A = a.toThunk
  }

  val consoleToPar: (Console ~> Par) = new (Console ~> Par) {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  val consoleToReader: (Console ~> ConsoleReader) = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]): ConsoleReader[A] = a.toReader
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def runConsoleReader[A](a: Free[Console, A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](a)(consoleToReader)

  def runConsole[A](a: Free[Console, A]): A = {
    runTrampoline(translate(a)(consoleToFunction0))
  }

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {

    def flatMap[A, B](a: () => A)(f: A => () => B): () => B =
      f(a())

    def unit[A](a: => A): () => A = () => a

  }

  implicit val par0Monad: Monad[Par] = new Monad[Par] {

    def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }

    def unit[A](a: => A): Par[A] = Par.unit(a)

  }

  val app: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console")
    ln <- Console.readLn
  } yield ln
  
}
