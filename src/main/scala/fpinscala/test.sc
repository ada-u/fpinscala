import fpinscala.monad.Monad
import fpinscala.monoid.Monoid

case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x]})#f] {

    override def unit[A](a: => A): Reader[R, A] =
      Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r =>
        f(ma.run(r)).run(r)
      }
  }
}

val reader: Reader[Int, Int] = Reader.readerMonad[Int].flatMap(Reader[Int, Int](x => x + 1)) {
  a =>
    Reader(r => a + r)
}
//reader.run(1)


case class Iteration[A](a: A, f: A => A, n: Int) {
  def foldMap[B](g: A => B)(M: Monoid[B]): B = {
    def iterate(n: Int, b: B, c: A): B =
      if (n <= 0) b else iterate(n - 1, g(c), f(a))
    iterate(n, M.zero, a)
  }
}


val itr = Iteration[Int](1, x => x, 10)

val result = itr.foldMap(x => x)(Monoid.intAddition)

println(result)




