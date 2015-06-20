import fpinscala.io._

import scala.annotation.tailrec

object Main extends App {

  def printLine(msg: String): TailRec[Unit] =
    Suspend(() => println(msg))

  val p = TailRec.forever(printLine("Still going..."))
  run(p)

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
