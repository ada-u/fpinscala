package fpinscala.arrow.syntax

import fpinscala.arrow.Arrow

import scala.language.higherKinds

final class ArrowOps[F[_, _], A, B](val self: F[A, B])(implicit val F: Arrow[F]) {

   def ***[C, D](k: F[C, D]): F[(A, C), (B, D)] = F.split(self, k)

}
