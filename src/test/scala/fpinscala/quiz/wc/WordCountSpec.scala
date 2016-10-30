package fpinscala.quiz.wc

import fpinscala.monoid.Monoid
import fpinscala.testing.{Gen, Prop}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class WordCountSpec extends FlatSpec with DiagrammedAssertions {

  "WordCountMonoid" should "satisfy Monoid Laws" in {
    Prop.run {
      val stubGen = Gen.string.map(Stub)(10)
      val partGen = for {
        n <- Gen.smallPositiveInt
        s <- Gen.string(n)
        m <- Gen.smallPositiveInt.map(_ + n)
        l <- Gen.choose(0, m)
        r <- Gen.string(l)
      } yield Part(s, l, r)
      Monoid.monoidLaws[WordCount](WordCount.wordCountMonoid,  Gen.union(stubGen, partGen))
    }
  }

  "WordCount" should "count words correctly" in {
    val wc = (s: String) => if (s.trim == "") 0 else s.trim.split("""\s+""").size
    Prop.run {
      Prop.forAll(Gen.textN(1000)) { s =>
        wc(s) == WordCount.count(s)
      }
    }
  }

}
