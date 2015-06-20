package fpinscala.applicative

import fpinscala.option.MyOption
import org.scalatest.FunSuite

class ApplicativeSpec extends FunSuite {

  val F: Applicative[MyOption] = new Applicative[MyOption] {

    override def map2[A, B, C](fa: MyOption[A], fb: MyOption[B])(f: (A, B) => C): MyOption[C] =
      for(a <- fa ; b <- fb) yield f(a, b)

    override def unit[A](a: => A): MyOption[A] =
      MyOption(a)

  }

}
