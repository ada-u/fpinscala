package fpinscala.category.instance

import fpinscala.category.Split

object SplitInstance {

  implicit val function1Instance: Split[Function1] = new Split[Function1] {

    def split[A, B, C, D](f: A => B, g:C => D): ((A, C)) => (B, D) = {
      case (a, c) => (f(a), g(c))
    }

    def compose[A, B, C](f: (B) => C, g: (A) => B): A => C =
      ComposeInstance.function1Instance.compose(f, g)

  }

}
