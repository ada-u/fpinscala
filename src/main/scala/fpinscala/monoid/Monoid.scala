package fpinscala.monoid

import fpinscala.collection.list.MyList
import fpinscala.option.MyOption
import fpinscala.pallarelism.nonblocking.Nonblocking.Par
import fpinscala.pallarelism.nonblocking.Nonblocking.Par.toParOps
import fpinscala.semigroup.Semigroup
import fpinscala.testing.{ Gen, Prop }


trait Monoid[A] extends Semigroup[A] {

  def zero: A

}

object Monoid {

  implicit val stringMonoid = new Monoid[String] {

    override def append(a1: String, a2: String): String =
      a1 + a2

    override def zero: String =
      ""

  }

  def listMonoid[A] = new Monoid[MyList[A]] {

    override def append(a1: MyList[A], a2: MyList[A]): MyList[A] =
      a1 ++ a2

    override def zero: MyList[A] =
      MyList.empty[A]

  }

  implicit val intAddition: Monoid[Int] = new Monoid[Int] {

    override def append(a1: Int, a2: Int): Int =
      a1 + a2

    override def zero: Int = 0

  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    override def append(a1: Int, a2: Int): Int =
      a1 * a2

    override def zero: Int = 1

  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    override def append(x: Boolean, y: Boolean): Boolean =
      x || y

    override def zero: Boolean = false

  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    override def append(x: Boolean, y: Boolean): Boolean =
      x && y

    override def zero: Boolean = true

  }

  def optionMonoid[A]: Monoid[MyOption[A]] = new Monoid[MyOption[A]] {

    override def append(x: MyOption[A], y: MyOption[A]): MyOption[A] =
      x orElse y

    override def zero: MyOption[A] =
      MyOption.empty[A]

  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    override def append(f: A => A, g: A => A): A => A =
      f compose g

    override def zero: A => A =
      identity[A]

  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def append(x: (A, B), y: (A, B)): (A, B) =
      (a.append(x._1, y._1), b.append(x._2, y._2))

    override def zero: (A, B) =
      (a.zero, b.zero)

  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def append(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, v.append(a.getOrElse(k, v.zero), b.getOrElse(k, v.zero)))
      }

    override def zero: Map[K, V] = Map[K, V]().empty

  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {

    override def append(f: A => B, g: A => B): A => B =
      a => b.append(f(a), g(a))

    override def zero: A => B =
      a => b.zero

  }

  def dual[A](monoid: Monoid[A]) = new Monoid[A] {

    override def append(x: A, y: A): A =
      monoid.append(y, x)

    override def zero: A =
      monoid.zero

  }

  def concatenate[A](as: MyList[A], monoid: Monoid[A]): A =
    as.foldLeft(monoid.zero)(monoid.append)

  def foldMap[A, B](as: MyList[A], monoid: Monoid[B])(f: A => B): B =
    as.foldLeft(monoid.zero)((acc, a) => monoid.append(acc, f(a)))

  def foldMapV[A, B](v: IndexedSeq[A], monoid: Monoid[B])(f: A => B): B =
    if (v.isEmpty) monoid.zero
    else if (v.length == 1) f(v(0))
    else {
      val (left, right) = v.splitAt(v.length / 2)
      monoid.append(foldMapV(left, monoid)(f), foldMapV(right, monoid)(f))
    }

  def foldLeft[A, B](as: MyList[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldRight[A, B](as: MyList[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def par[A](monoid: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {

    override def append(a1: Par[A], a2: Par[A]): Par[A] =
      a1.map2(a2)(monoid.append)

    override def zero: Par[A] =
      Par.unit(monoid.zero)

  }

  def parFoldMap[A, B](v: IndexedSeq[A], monoid: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(monoid))(b => Par.lazyUnit(b))
    }

  def monoidLaws[A](monoid: Monoid[A], gen: Gen[A]): Prop = {
    val associativity = Prop.forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, x)
    ) { p =>
      monoid.append(p._1, monoid.append(p._2, p._3)) == monoid.append(monoid.append(p._1, p._2), p._3)
    }
    val identity = Prop.forAll(gen) { a =>
      monoid.append(a, monoid.zero) == monoid.append(monoid.zero, a)
    }
    associativity && identity
  }
}