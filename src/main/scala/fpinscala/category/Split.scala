package fpinscala.category

import scala.language.higherKinds

trait Split[=>: [_, _]] extends Compose[=>:] {

  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A, C) =>: (B, D)

}
