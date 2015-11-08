package fpinscala.category.syntax

import fpinscala.category.Compose

import scala.language.higherKinds

trait ComposeOps[F[_, _], A, B] {

  val F: Compose[F]

  val self: F[A, B]

  final def <<<[C](x: F[C, A]): F[C, B] =
    F.compose(self, x)

  final def >>>[C](x: F[B, C]): F[A, C] =
    F.compose(x, self)

}

object ToComposeOps {

  implicit def ToComposeOps[F[_, _], A, B](v: F[A, B])(implicit F0: Compose[F]) =
    new ComposeOps[F, A, B] {
      val F: Compose[F] = F0
      val self: F[A, B] = v
    }

}

