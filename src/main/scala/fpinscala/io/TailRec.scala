package fpinscala.io

import fpinscala.monad.Monad

sealed trait TailRec[A] {

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {

  def unit[A](a: => A): TailRec[A] =
    Return(a)

  def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
    a.flatMap(f)

}