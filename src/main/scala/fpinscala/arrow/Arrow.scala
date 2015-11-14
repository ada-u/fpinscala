package fpinscala.arrow

import fpinscala.category.{Split, Category}

import scala.language.higherKinds

trait Arrow[=>: [_, _]] extends Split[=>:] with Category[=>:] {

  def id[A]: A =>: A

  def arrow[A, B](f: A => B): A =>: B

  def first[A, B, C](f: A =>: B): ((A, C) =>: (B, C))

  def split[A, B, C, D](f: A =>: B, g: C =>: D): ((A, C) =>: (B, D))

  def combine[A, B, C](f: A =>: B, g: A =>: C): (A =>: (B, C)) =
    compose(split(f, g), arrow((a: A) => (a, a)))

}

