package fpinscala.state

import fpinscala.collection.list.MyList

trait RNG {

  def int: (Int, RNG)

  def positiveInt: (Int, RNG) =
    int match {
      case (x, r) if x == Int.MinValue  => r.positiveInt
      case (x, r) if x == 0             => r.positiveInt
      case (x, r)                       => (math.abs(x), r)
    }

  def double: (Double, RNG) = RNG.double.run(this)

  def ints(count: Int): (MyList[Int], RNG) = RNG.ints(count).run(this)

}

object RNG {

  type Rand[A] = State[RNG, A]

  case class SimpleRNG(seed: Long) extends RNG {
    def int: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = State(_.int)

  val positiveInt: Rand[Int] = State(_.positiveInt)

  val double: Rand[Double] = positiveInt.map(n => (n.toDouble - 1) / Int.MaxValue)

  val boolean: Rand[Boolean] = int.map(n => if (n % 2 == 0) true else false)

  def nonNegativeInt: Rand[Int] =
    int.map { i => if (i < 0) -(i + 1) else i }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    ra.map2(rb)((_, _))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def unit[A](a: A): Rand[A] =
    State.unit(a)

  def randomPair: Rand[(Int, Int)] =
    both(int, int)

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - (i % 2))

  def ints(count: Int): Rand[MyList[Int]] =
    State.sequence(MyList.fill(count)(int))

}