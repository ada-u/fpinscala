package fpinscala.parser

import ReferenceTypes._
import fpinscala.either.{ MyRight, MyLeft, MyEither }
import fpinscala.collection.list.MyList
import scala.util.matching.Regex

object ReferenceTypes {

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    def extract: MyEither[ParseError, A] = this match {
      case Failure(e, _) => MyLeft(e)
      case Success(a, _) => MyRight(a)
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }

}


object Reference extends Parsers[Parser] {

  def string(w: String): Parser[String] =
    s => {
      val i = firstNonmatchingIndex(s.input, w, s.offset)
      if (i == -1)
        Success(w, w.length)
      else
        Failure(s.advanceBy(i).toError("Expected: " + "'" + s + "'"), i != 0)
    }

  def regex(r: Regex): Parser[String] = {
    val message = "regex " + r
    s => r.findPrefixOf(s.input) match {
      case None => Failure(s.toError(message), isCommitted = false)
      case Some(m) => Success(m, m.length)
    }
  }

  override def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) =>
        g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_, _) => f
    }

  override def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e, false) => y(s)
      case r => r
    }

  override final def many[A](p: Parser[A]): Parser[MyList[A]] =
    s => {
      def go(acc: MyList[A], p: Parser[A], offset: Int): Result[MyList[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a, n) =>
            go(a :: acc, p, offset + n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(acc, offset)
        }
      }
      go(MyList.empty[A], p, 0)
    }

  override def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_, n) =>
        Success(s.input.substring(s.column, s.column + n), s.column + n)
      case f@Failure(_, _) => f
    }

  def succeed[A](a: A): Parser[A] =
    s => Success(a, 0)

  override def label[A](message: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(message))

  override def scope[A](message: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, message))

  override def run[A](p: Parser[A])(input: String): MyEither[ParseError, A] =
    p(Location(input)).extract

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit
}