package fpinscala.quiz.wc

import fpinscala.monoid.Monoid

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCount {

  val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {

    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    override def zero: WordCount =
      Stub("")

  }

  def count(string: String): Int = {

    def wc(c: Char): WordCount =
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def unstub(s: String): Int =
      s.length.min(1)

    Monoid.foldMapV(string.toIndexedSeq, wordCountMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }

  }

}