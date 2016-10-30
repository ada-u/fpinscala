package fpinscala.state

import State._
import fpinscala.collection.list.MyList

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State {
      s =>
        val (a, newState) = run(s)
        f(a).run(newState)
    }
  }

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, S] = State(_ => (s, s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S, A](sas: MyList[State[S, A]]): State[S, MyList[A]] =
    sas.foldRight(unit[S, MyList[A]](MyList[A]()))((f, acc) => f.map2(acc)(_ :: _))

}
