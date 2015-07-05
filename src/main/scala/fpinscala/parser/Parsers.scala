package fpinscala.parser

import java.util.regex.Pattern

import fpinscala.either.MyEither
import fpinscala.collection.list.MyList
import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): MyEither[ParseError, A]

  def attempt[A](p: Parser[A]): Parser[A]

  def skipLeft[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipRight[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def whiteSpace: Parser[String] =
    "\\s*".r

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whiteSpace

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]): Parser[A] =
    start *> p <* stop

  def separate[A](p: Parser[A], p2: Parser[Any]): Parser[MyList[A]] =
    separate1(p, p2) or succeed(MyList.empty[A])

  def separate1[A](p: Parser[A], p2: Parser[Any]): Parser[MyList[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def through(s: String): Parser[String] =
    (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] =
    string("\"") *> through("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
    token(quoted)

  private def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for { a <- p ; b <- p2 } yield (a, b)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f.andThen(succeed))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for { a <- p ;  b <- p2 } yield f(a, b)

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def listOfN[A](n: Int, p: Parser[A]): Parser[MyList[A]] =
    if (n < 1) succeed(MyList.empty[A]) else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[MyList[A]] =
    map2(p, many(p))(_ :: _) or succeed(MyList.empty[A])

  //def many1[A](p: Parser[A]): Parser[MyList[A]]

  def eof: Parser[String] =
    regex("\\z".r)

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  def label[A](message: String)(p: Parser[A]): Parser[A]

  def scope[A](message: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def *>[B](p2: => Parser[B]) = self.skipLeft(p, p2)
    def <*(p2: => Parser[Any]) = self.skipRight(p, p2)
    def **[B](p2: => Parser[B]) = self.product(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def token = self.token(p)
    def separate(separator: Parser[Any]): Parser[MyList[A]] = self.separate(p, separator)
    def slice: Parser[String] = self.slice(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[MyList[A]] = self.many(p)
    def label(message: String): Parser[A] = self.label(message)(p)
    def scope(message: String): Parser[A] = self.scope(message)(p)
    def product[B](p2: => Parser[B]) = self.product(p, p2)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

  }
}