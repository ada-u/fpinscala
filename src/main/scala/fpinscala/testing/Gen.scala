package fpinscala.testing

import fpinscala.collection.list.MyList
import fpinscala.state.{RNG, State}

trait Cogen[-A] {
  def sample(a: A, rng: RNG): RNG
}

object Cogen {

  def apply[A](f: (RNG, A) => RNG): Cogen[A] =
    new Cogen[A] {
      def sample(a: A, rng: RNG): RNG = f(rng, a)
    }

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[MyList[A]] =
    size.flatMap(n => listOfN(n))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
  // State[RNG, A]をState[RNG, B]にして、Genで包む
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[MyList[A]] =
    Gen.listOfN(size, this)

  def unsized: SGen[A] =
    SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

}

object Gen {

  val boolean: Gen[Boolean] =
    Gen(RNG.boolean)

  val string: SGen[String] =
    SGen(stringN)

  val whitespaceChar: Gen[Char] =
    listOf1(Gen.union(choose(9, 10), unit(32)))(1).map(_.head.toChar)

  val nonWhitespaceChar: Gen[Char] =
    choose(33, 127).map(_.toChar)

  val text: SGen[String] = SGen(textN)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOf[A](g: Gen[A]): SGen[MyList[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[MyList[A]] =
    SGen(n => g.listOfN(n.max(1)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { rng =>
      val (seed, rng2) = rng.int
      val fn: (String) => A = (s: String) =>
        // gの結果を、引数(String)に依存させる
        g.sample.run(RNG.SimpleRNG(seed.toLong ^ s.##.toLong))._1
      (fn, rng2)
    }
  }

  def fn[A, B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State { rng =>
      val (_, rng2) = rng.int
      val f = (a: A) => {
        out.sample.run(in.sample(a, rng))._1
      }
      (f, rng2)
    }
  }

  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  val smallPositiveInt: Gen[Int] = Gen.choose(0, 10)

  def intBoolean(g: Gen[Boolean]): Gen[Int => Boolean] = {
    g.map(b => i => b)
  }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g.map(i => s => i)

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeInt.map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[MyList[A]] =
    Gen(State.sequence(MyList.fill(n)(g.sample)))

  def textN(n: Int): Gen[String] =
    listOfN(n, weighted(whitespaceChar -> 0.1, nonWhitespaceChar -> 0.9)).map(_.mkString)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(RNG.double.flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

}