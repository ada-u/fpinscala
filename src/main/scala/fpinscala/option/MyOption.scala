package fpinscala.option

import fpinscala.collection.list.{MyNil, MyList}

sealed trait MyOption[+A]  {

  def get: A

  def isEmpty: Boolean

  def map[B](f: A => B): MyOption[B] =
    if (isEmpty) MyNone else MySome(f(get))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    if (isEmpty) MyNone else f(get)

  def getOrElse[B >: A](elseValue: B): B =
    if (isEmpty) elseValue else get


  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if (f(a)) MySome(a) else MyNone)

  def orElse[B >: A](elseValue: => MyOption[B]): MyOption[B] =
    map(MySome(_)).getOrElse(elseValue)

  def toList: MyList[A] =
    if (isEmpty) MyNil else MyList(get)

}

case object MyNone extends MyOption[Nothing] {

  def get = throw new NoSuchElementException("None.get")

  def isEmpty = true

}

case class MySome[+A](x: A) extends MyOption[A] {

  def get = x

  def isEmpty = false
}

object MyOption {

  def empty[A]: MyOption[A] = MyNone

  def apply[A](x: A): MyOption[A] =
    if (x == null) MyNone else MySome(x)

}