package fpinscala.category

import fpinscala.category.syntax.ComposeSyntax

import scala.language.higherKinds

/*
trait Compose[F[_, _]] {

  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]


}
*/

trait Compose[=>: [_, _]] {

  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C

  val syntax = new ComposeSyntax[=>:] {
    def F = Compose.this
  }

}




