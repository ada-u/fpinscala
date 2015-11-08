package fpinscala.category.instance

import fpinscala.category.Compose

object ComposeInstance {

  implicit val function1Instance: Compose[Function1] = new Compose[Function1] {
    def compose[A, B, C](f: (B) => C, g: (A) => B): (A) => C = f compose g
  }

}
