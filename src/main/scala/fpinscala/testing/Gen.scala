package fpinscala.testing

import fpinscala.collection.list.MyList
import fpinscala.state.{State, RNG}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
  // State[RNG, A]をState[RNG, B]にして、Genで包む
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[MyList[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[MyList[A]] =
    size.flatMap(n => listOfN(n))

  def unsized: SGen[A] =
    SGen(_ => this)
  
  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))

}

object Gen {

  val boolean: Gen[Boolean] =
    Gen(RNG.boolean)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeInt.map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOf[A](g: Gen[A]): SGen[MyList[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[MyList[A]] =
    SGen(n => g.listOfN(n.max(1)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[MyList[A]] =
    Gen(State.sequence(MyList.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(RNG.double.flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

}