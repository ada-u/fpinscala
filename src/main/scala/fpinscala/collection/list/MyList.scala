package fpinscala.collection.list

import fpinscala.option.{MySome, MyNone, MyOption}

import scala.annotation.tailrec

sealed trait MyList[+A] {

  def isEmpty: Boolean

  def length: Int =
    foldLeft(0)((acc, _) => acc + 1)

  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil => z
    case MyCons(x, xs) => xs.foldLeft(f(z, x))(f)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B =
    reverse.foldLeft(z)((b, a) => f(a, b))

  def reduceLeft[B >: A] (f: (B, A) => B): B =
    this match {
      case MyNil =>
        throw new UnsupportedOperationException()
      case MyCons(x, xs) =>
        xs.foldLeft(x.asInstanceOf[B])(f)
    }

  def reduceRight[B >: A](f: (A, B) => B): B =
    this match {
      case MyNil =>
        throw new UnsupportedOperationException
      case MyCons(x, xs) =>
        xs.foldRight(x.asInstanceOf[B])(f)
    }

  def reduce[B >: A](f: (B, B) => B): B = reduceLeft(f)

  def max[B >: A](implicit cmp: Ordering[B]): A =
    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)

  def min[B >: A](implicit cmp: Ordering[B]): A =
    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)

  def exists(p: A => Boolean): Boolean = this match {
    case MyNil => false
    case MyCons(x, xs) if p(x) => true
    case MyCons(_, xs) => xs.exists(p)
  }

  def ::[B >: A](b: B): MyList[B] =
    MyCons(b, this)

  def reverse: MyList[A] =
    foldLeft(MyList[A]())((acc, h) => h :: acc)

  def ++[B >: A](b: MyList[B]): MyList[B] =
    foldRight(b)(_ :: _)

  def map[B](f: A => B): MyList[B] =
    foldRight(MyList[B]())((x, xs) => f(x) :: xs)

  def flatMap[B](f: A => MyList[B]): MyList[B] =
    foldRight(MyList[B]())(f(_) ++ _)

  def find(f: A => Boolean): MyOption[A] = this match {
    case MyNil => MyNone
    case MyCons(x, xs) if f(x) => MySome(x)
    case MyCons(_, xs) => xs.find(f)
  }

  def startsWith[B >: A](prefix: MyList[B]): Boolean = {

    def loop(list: MyList[A], prefix: MyList[B]): Boolean = (list, prefix) match {
      case (_, MyNil) => true
      case (MyCons(x, xs), MyCons(x2, xs2)) if x == x2 => loop(xs, xs2)
      case _ => false
    }

    (this, prefix) match {
      case (MyCons(_, _), MyNil) => false
      case _ => loop(this, prefix)
    }
  }

  def filter(f: A => Boolean): MyList[A] =
    foldRight(MyList[A]())((a, acc) => if (f(a)) a :: acc else acc)

  def foldLeftViaFoldRight[B](z: B)(f: (B, A) => B): B =
    foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B =
    foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def toIndexSeq: IndexedSeq[A] =
    foldLeft(IndexedSeq.empty[A])(_.+:(_))

  def lastOption: MyOption[A] = this.reverse match {
    case MyNil => MyNone
    case MyCons(x, _) => MySome(x)
  }

  def mkString(start: String, separator: String, end: String): String =
    foldLeft(start)((acc, a) => acc ++ separator ++ a.toString) ++ end

  def mkString(separator: String): String =
    mkString("", separator, "")

  def mkString: String =
    mkString("")

}

case object MyNil extends MyList[Nothing] {

  def isEmpty: Boolean = true

}

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A] {

  def isEmpty: Boolean = false

}

object MyList {

  implicit def toMapOps[K, V](list: MyList[(K, V)]): ToMapOps[K, V] =
    ToMapOps(list)

  case class ToMapOps[K, V](list: MyList[(K, V)]) {
    def toMap: Map[K, V] =
      list.foldLeft(Map.empty[K, V])(_ + _)
  }

  def empty[A]: MyList[A] = MyNil

  def apply[A](as: A*): MyList[A] = {
    @tailrec
    def loop(acc: MyList[A], as: A*): MyList[A] =
      if (as.isEmpty) acc else loop(MyCons(as.head, acc), as.tail: _*)

    loop(MyNil, as.reverse: _*)
  }

  def fill[A](n: Int)(element: => A): MyList[A] =
    if (n < 0) MyNil else apply(Seq.fill(n)(element): _*)

}