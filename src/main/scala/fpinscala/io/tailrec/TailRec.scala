package fpinscala.io.tailrec

import fpinscala.io.tailrec
import fpinscala.monad.Monad

import scala.annotation.tailrec

sealed trait TailRec[A] {

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    tailrec.FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (tailrec.Return(_)))
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {

  def unit[A](a: => A): TailRec[A] =
    tailrec.Return(a)

  def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
    a.flatMap(f)

}

object Sandbox {

  def printLine(msg: String): TailRec[Unit] =
    Suspend(() => println(msg))

  //val p = TailRec.forever(printLine("Still going..."))
  //run(p)

  println(
    run(
      FlatMap(
        FlatMap(
          Return(10), (x: Int) => Return(x * 10)
        ),
        (y: Int) => Return(y * 2)
      )
    )
  )

  @tailrec
  def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
    }
  }
}