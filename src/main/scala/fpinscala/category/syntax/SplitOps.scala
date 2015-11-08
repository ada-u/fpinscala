package fpinscala.category.syntax

import fpinscala.category.Split

import scala.language.higherKinds

trait SplitOps[F[_, _], A, B] {

  val F: Split[F]

  val self: F[A, B]

  final def -*-[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)

}

object ToSplitOps {

  implicit def ToSplitOps[F[_, _], A, B](v: F[A, B])(implicit F0: Split[F]) =
    new SplitOps[F, A, B] {
      val F: Split[F] = F0
      val self: F[A, B] = v
    }

}
