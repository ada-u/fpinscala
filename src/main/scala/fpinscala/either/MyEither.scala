package fpinscala.either

trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(a) => f(a)
    case MyLeft(e) => MyLeft(e)
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]