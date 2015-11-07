package fpinscala.monoid

import scala.language.implicitConversions
import scalaz.syntax.ToSemigroupOps

trait MonoidOps[A] {

  val F: Monoid[A]

  val value: A

  def |+|(a2: A): A = F.append(value, a2)

}

object ToMonoidOps extends ToSemigroupOps {

  implicit def toMonoidOps[A: Monoid](a: A): MonoidOps[A] = new MonoidOps[A] {
    val value: A = a
    val F: Monoid[A] = implicitly[Monoid[A]]
  }

}