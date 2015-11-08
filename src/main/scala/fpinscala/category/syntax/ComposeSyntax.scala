package fpinscala.category.syntax

import fpinscala.category.Compose

import scala.language.{ higherKinds, implicitConversions }

trait ComposeSyntax[F[_, _]] {

  def F: Compose[F]

  implicit def ToComposeOps[A, B](v: F[A, B]): ComposeOps[F, A, B] =
    new ComposeOps[F, A, B] {
      val F: Compose[F] = ComposeSyntax.this.F
      val self: F[A, B] = v
    }

}
