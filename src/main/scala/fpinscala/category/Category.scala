package fpinscala.category

import fpinscala.category.syntax.CategorySyntax

import scala.language.higherKinds

trait Category[=>: [_, _]] extends Compose[=>:] {

  def id[A]: A =>: A

  val categorySyntax = new CategorySyntax[=>:] {
    def F: Category[=>:] = Category.this
  }

}