package fpinscala.testing

import fpinscala.option.MySome
import fpinscala.collection.stream.MyStream
import fpinscala.state.{ RNG, State }
import fpinscala.testing.Prop._

import scala.util.Try

case class TestFailedException(message: String = "Test failed.") extends Exception

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, _) => p.tag(e).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def apply(f: (TestCases, RNG) => Result): Prop = Prop {
    (_, n, rng) => f(n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
        throw TestFailedException()
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(MyStream.from(0).take(n)).map {
        case (a, i) => Try {
          if (f(a)) Passed else Falsified(a.toString, i)
        }.recover {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }.get
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      // 最低1つ以上のケースを用意する
      val casePerSize = (n + (max - 1)) / max
      val props: MyStream[Prop] = MyStream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) =>
          p.run(max, casePerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] =
    MyStream.unfold(rng)(rng => MySome(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
    generated an exception: ${e.getMessage}
    stack trace:\n ${e.getStackTrace.mkString("\n")}"""

}