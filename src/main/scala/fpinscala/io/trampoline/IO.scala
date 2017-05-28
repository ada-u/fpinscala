package fpinscala.io.trampoline

import fpinscala.monad.Monad

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

}

case class Return[A](a: A) extends IO[A]

case class Suspend[A](resume: () => A) extends IO[A]

case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {

  def unit[A](a: => A): IO[A] = Return(a)

  def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

  def suspend[A](a: => IO[A]) =
    Suspend(() => ()).flatMap { _ => a }

}
