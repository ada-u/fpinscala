package fpinscala.category.syntax

import fpinscala.category.Split

import scala.language.{ implicitConversions, higherKinds }

trait SplitSyntax[F[_, _]] {

  def F: Split[F]

  implicit def ToSplitOps[A, B](v: F[A, B]): SplitOps[F, A, B] =
    new SplitOps[F, A, B] {
      val F: Split[F] = SplitSyntax.this.F
      val self: F[A, B] = v
    }

}
