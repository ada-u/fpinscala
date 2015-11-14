package fpinscala.category.syntax

import fpinscala.category.Category

import scala.language.higherKinds

trait CategoryOps[F[_, _], A, B] extends ComposeOps[F, A, B] {

  val F: Category[F]

  val self: F[A, B]

}

object ToCategoryOps {

  implicit def ToCategoryOps[F[_, _], A, B](v: F[A, B])(implicit F0: Category[F]) =
    new CategoryOps[F, A, B] {
      val F: Category[F] = F0
      val self: F[A, B] = v
    }
}