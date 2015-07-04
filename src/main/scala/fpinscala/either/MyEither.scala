package fpinscala.either

import fpinscala.collection.list.{MyNil, MyCons, MyList}

trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(a) => f(a)
    case MyLeft(e) => MyLeft(e)
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for { a <- this ; b1 <- b } yield f(a, b1)

  def fold[X](fe: E => X, fa: A => X) = this match {
    case MyLeft(a) => fe(a)
    case MyRight(b) => fa(b)
  }

}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {

  def empty[E, A](a: A): MyEither[E, A] =
    MyRight(a)

  def traverse[E, A, B](es: MyList[A])(f: A => MyEither[E, B]): MyEither[E, MyList[B]] =
    es match {
      case MyNil => MyRight(MyNil)
      case MyCons(x, xs) => (f(x) map2 traverse(xs)(f))(_ :: _)
    }

  def sequence[E, A](es: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] =
    traverse(es)(identity)
}
