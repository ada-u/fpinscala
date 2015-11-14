package fpinscala.category.syntax

import fpinscala.category.Category

import scala.language.{ implicitConversions, higherKinds }

trait CategorySyntax[F[_,_]] {

  def F: Category[F]

  implicit def ToCategoryOps[A, B](v: F[A, B]): CategoryOps[F, A, B] =
    new CategoryOps[F, A, B] {
      val F: Category[F] = CategorySyntax.this.F
      val self: F[A, B] = v
    }

}
