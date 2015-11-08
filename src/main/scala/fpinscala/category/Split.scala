package fpinscala.category

import fpinscala.category.syntax.{ SplitSyntax, ComposeSyntax }

import scala.language.higherKinds

trait Split[=>: [_, _]] extends Compose[=>:] {

  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A, C) =>: (B, D)

  val splitSyntax = new SplitSyntax[=>:] {
    def F = Split.this
  }

}
