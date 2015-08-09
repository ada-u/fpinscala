package fpinscala.io.console

import java.util.concurrent.Executors

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

  val consoleToState: (Console ~> ConsoleState) = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]): ConsoleState[A] = a.toState
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def runConsoleReader[A](a: Free[Console, A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](a)(consoleToReader)

  def runConsoleState[A](a: Free[Console, A]): ConsoleState[A] =
    runFree[Console, ConsoleState, A](a)(consoleToState)

  def runConsole[A](a: Free[Console, A]): A = {
    runTrampoline(translate(a)(consoleToFunction0))
  }

  val app: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console")
    ln <- Console.readLn
  } yield ln

  def runApp: Option[String] = {
    //runConsole(app)
    //runConsoleFunction0(app)()
    Par.run(Executors.newCachedThreadPool)(runConsolePar(app))
  }

  // ---

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {

    def flatMap[A, B](a: () => A)(f: A => () => B): () => B =
      f(a())

    def unit[A](a: => A): () => A = () => a

  }

  implicit val par0Monad: Monad[Par] = new Monad[Par] {

    def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }

    def unit[A](a: => A): Par[A] = Par.unit(a)

  }
  
}
