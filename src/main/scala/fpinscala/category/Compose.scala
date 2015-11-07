package fpinscala.category

import scala.language.higherKinds

/*
trait Compose[F[_, _]] {

  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]


}
*/

trait Compose[=>: [_, _]] {

  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C

}

trait ComposeOps[F[_, _], A, B] {

  val F: Compose[F]

  val self: F[A, B]

  final def <<<[C](x: F[C, A]): F[C, B] = F.compose(self, x)

  final def >>>[C](x: F[B, C]): F[A, C] = F.compose(x, self)

}


