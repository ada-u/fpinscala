package fpinscala.collection.list

import fpinscala.monad.Monad
import fpinscala.monoid.Monoid
import fpinscala.option.{ MySome, MyNone, MyOption }

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag

sealed trait MyList[+A] { self =>

  def head: A

  def isEmpty: Boolean

  def length: Int =
    foldLeft(0)((acc, _) => acc + 1)

  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil => z
    case MyCons(x, xs) => xs.foldLeft(f(z, x))(f)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B =
    foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def reduceLeft[B >: A] (f: (B, A) => B): B =
    this match {
      case MyNil =>
        throw new UnsupportedOperationException
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

  def sum[B >: A](implicit monoid: Monoid[B]): B =
    foldRight(monoid.zero)(monoid.append)

  def takeWhile(p: A => Boolean): MyList[A] = this match {
    case MyCons(h, t) if p(h) => MyCons(h, t.takeWhile(p))
    case _ => MyNil
  }

  def forall(p: A => Boolean): Boolean = this match {
    case MyNil => false
    case MyCons(x ,xs) if !p(x) => false
    case MyCons(x, xs) => xs.forall(p)
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = this match {
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

  @tailrec
  final def find(f: A => Boolean): MyOption[A] = this match {
    case MyNil => MyNone
    case MyCons(x, xs) if f(x) => MySome(x)
    case MyCons(_, xs) => xs.find(f)
  }

  def startsWith[B >: A](prefix: MyList[B]): Boolean = {

    @tailrec
    def loop(list: MyList[A], prefix: MyList[B]): Boolean = (list, prefix) match {
      case (_, MyNil) => true
      case (MyNil, _) => false
      case (MyCons(x, xs), MyCons(x2, xs2)) if x == x2 => loop(xs, xs2)
    }

    (this, prefix) match {
      case (MyCons(_, _), MyNil) => false
      case _ => loop(this, prefix)
    }
  }

  def filter(p: A => Boolean): MyList[A] =
    foldRight(MyList[A]())((a, acc) => if (p(a)) a :: acc else acc)

  sealed class WithFilter(p: A => Boolean) {

    def map[B](f: A => B): MyList[B] =
      self.foldRight(MyList[B]())((a, acc) => if (p(a)) f(a) :: acc else acc)

    def flatMap[B](f: A => MyList[B]): MyList[B] =
      foldRight(MyList[B]())((a, acc) => if (p(a)) f(a) ++ acc else acc)

    def withFilter(q: A => Boolean): WithFilter =
      WithFilter(q)

  }

  object WithFilter {
    def apply(p: A => Boolean): WithFilter =
      new WithFilter(p)
  }

  def withFilter(p: A => Boolean): WithFilter =
    WithFilter(p)

  def foldLeftViaFoldRight[B](z: B)(f: (B, A) => B): B =
    foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B =
    foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def toIndexedSeq: IndexedSeq[A] =
    foldRight(IndexedSeq.empty[A])(_ +: _)

  def toArray[B >: A : ClassTag]: Array[B] =
    foldRight(Array.empty[B])(_ +: _)

  def toList: List[A] =
    foldRight(List.empty[A])(_ :: _)

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

  override final def toString: String =
    mkString

}

case object MyNil extends MyList[Nothing] {

  def head = throw new NoSuchElementException

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

  def apply[A](as: A*): MyList[A] =
    as.foldRight(empty[A])(MyCons(_, _))

  def fill[A](n: Int)(element: => A): MyList[A] =
    if (n < 0) MyNil else apply(Seq.fill(n)(element): _*)

  implicit val listMonadInstance = new Monad[MyList] {

    override def unit[A](a: => A): MyList[A] = MyList(a)

    override def flatMap[A, B](ma: MyList[A])(f: A => MyList[B]): MyList[B] = ma.flatMap(f)

  }

}