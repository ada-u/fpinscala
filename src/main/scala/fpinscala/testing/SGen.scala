package fpinscala.testing

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize.andThen(_.map(f)))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize.andThen(_.flatMap(f)))

  /*
  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))*/
}