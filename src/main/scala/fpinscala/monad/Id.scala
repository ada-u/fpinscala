package fpinscala.monad

case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

}

object Id {

  val idMonad = new Monad[Id] {

    override def unit[A](a: => A): Id[A] =
      Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)

  }
}
