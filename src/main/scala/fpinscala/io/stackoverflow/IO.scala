package fpinscala.io

import fpinscala.monad.Monad

sealed trait IO[A] {
  self =>

  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] = {
    // f: _ => Monad#forever(PrintLine("Still going..."))
    new IO[B] {
      // 1. self.runでprintln("Still going...")
      // 2. fを評価、forever中でIO#flatMap() -> 新しいIOのインスタンスが生成
      // 3. 新しいIOのインスタンス.run
      // 4. 1-3の繰り返しでスタックがpushされていく
      def run = f(self.run).run
    }
  }
}

object IO extends Monad[IO] {
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }
}
