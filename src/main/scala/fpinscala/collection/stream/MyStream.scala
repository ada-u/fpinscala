package fpinscala.collection.stream

import fpinscala.collection.list.MyList
import fpinscala.option.{MyNone, MyOption, MySome}

import scala.annotation.tailrec

import MyStream._

sealed trait MyStream[+A] {

  def headOption: MyOption[A] =
    foldRight(MyNone: MyOption[A])((x, _) => MySome(x))

  def toList: MyList[A] = {
    @tailrec
    def loop(acc: MyList[A], xs: MyStream[A]): MyList[A] = xs match {
      case MyEmpty => acc
      case MyCons(h, t) => loop(h() :: acc, t())
    }
    loop(MyList.empty[A], this).reverse
  }

  def take(n: Int): MyStream[A] = {
    require(n >= 0)
    if (n ==  0) MyStream.empty else this match {
      case MyEmpty => MyStream.empty
      case MyCons(x, _) if n == 1 => cons(x(), MyEmpty)
      case MyCons(x, xs) => cons(x(), xs().take(n - 1))
    }
  }

  def drop(n: Int): MyStream[A] = {
    @tailrec
    def loop(acc: MyStream[A], n: Int): MyStream[A] =
      if (n <= 0) acc else acc match {
        case MyEmpty => empty[A]
        case MyCons(_, xs) => loop(xs(), n - 1)
      }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): MyStream[A] =
    foldRight(empty[A])((x, xs) => if (p(x)) cons(x, xs) else empty[A])

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def append[B >: A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((x, xs) => cons(x, xs))

  def map[B](f: A => B): MyStream[B] =
    foldRight(empty[B])((x, xs) => cons(f(x), xs))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])((x, xs) => f(x).append(xs))

  def filter(p: A => Boolean): MyStream[A] =
    foldRight(empty[A])((x, xs) => if (p(x)) cons(x, xs) else xs)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case MyEmpty => z
    case MyCons(x, xs) => f(x(), xs().foldRight(z)(f))
  }

  def zipWith[B, C](s2: MyStream[B])(f: (A, B) => C): MyStream[C] =
    unfold((this, s2)) {
      case (MyCons(h1,t1), MyCons(h2,t2)) =>
        MySome((f(h1(), h2()), (t1(), t2())))
      case _ => MyNone
    }

  def zip[B](s2: MyStream[B]): MyStream[(A, B)] =
    zipWith(s2)((_, _))


  def zipAll[B](s2: MyStream[B]): MyStream[(MyOption[A],MyOption[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: MyStream[B])(f: (MyOption[A], MyOption[B]) => C): MyStream[C] =
    MyStream.unfold((this, s2)) {
      case (MyEmpty, MyEmpty) => MyNone
      case (MyCons(h, t), MyEmpty) => MySome(f(MySome(h()), MyOption.empty[B]) -> (t(), empty[B]))
      case (MyEmpty, MyCons(h, t)) => MySome(f(MyOption.empty[A], MySome(h())) -> (empty[A] -> t()))
      case (MyCons(h1, t1), MyCons(h2, t2)) => MySome(f(MySome(h1()), MySome(h2())) -> (t1() -> t2()))
    }

  @tailrec
  final def find(f: A => Boolean): MyOption[A] = this match {
    case MyEmpty => MyNone
    case MyCons(h, t) => if (f(h())) MySome(h()) else t().find(f)
  }

}

case object MyEmpty extends MyStream[Nothing]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

  def empty[A]: MyStream[A] = MyEmpty

  def cons[A](h: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val head = h
    lazy val tail = t
    MyCons(() => head, () => tail)
  }

  def apply[A](as: A*): MyStream[A] = {
    @tailrec
    def loop(acc: MyStream[A], as: A*): MyStream[A] = {
      if (as.isEmpty)
        acc
      else
        loop(cons(as.head, acc), as.tail: _*)
    }
    loop(MyEmpty, as.reverse: _*)
  }

  def constant[A](a: A): MyStream[A] =
    unfold(a) { _ => MySome(a, a) }

  def from(n: Int): MyStream[Int] =
    unfold(n) { n => MySome(n, n + 1) }

  def fibs: MyStream[Int] =
    unfold((0, 1)) { case (n0, n1) => MySome((n0, (n1, n0 + n1))) }

  def unfold[A, S](z: S)(f: S => MyOption[(A, S)]): MyStream[A] =
    f(z) match {
      case MyNone => empty
      case MySome((a, s)) => cons(a, unfold(s)(f))
    }

}