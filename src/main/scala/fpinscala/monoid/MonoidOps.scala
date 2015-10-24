package fpinscala.monoid

import scala.language.implicitConversions

trait MonoidOps[A] {

  val F: Monoid[A]

  val value: A

  def |+|(a2: A): A = F.op(value, a2)

}

object ToMonoidOps {

  implicit def toMonoidOps[A: Monoid](a: A): MonoidOps[A] = new MonoidOps[A] {
    val value: A = a
    val F: Monoid[A] = implicitly[Monoid[A]]
  }

}