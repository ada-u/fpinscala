package fpinscala.semigroup

import scala.language.implicitConversions

trait Semigroup[A] {

  def append(a1: A, a2: A): A

}

trait SemigroupOps[A] {

  val F: Semigroup[A]

  val self: A

  def mappend(other: A) = F.append(self, other)

  // alias
  def |+|(a2: A): A = F.append(self, a2)

}

object ToSemigroupOps {

  implicit def toSemigroupOps[A: Semigroup](a: A): SemigroupOps[A] = new SemigroupOps[A] {
    val self: A = a
    val F: Semigroup[A] = implicitly[Semigroup[A]]
  }

}