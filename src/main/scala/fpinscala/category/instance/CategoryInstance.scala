package fpinscala.category.instance

import fpinscala.category.Category

object CategoryInstance {

  implicit val function1Instance: Category[Function1] = new Category[Function1] {

    def id[A]: (A) => A = a => a

    def compose[A, B, C](f: (B) => C, g: (A) => B): (A) => C =
      ComposeInstance.function1Instance.compose(f, g)
  }

}
