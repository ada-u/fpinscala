import fpinscala.category.instance.ComposeInstance._
import fpinscala.collection.stream.MyStream
import fpinscala.option.{ MyNone, MySome }
import fpinscala.streamingio.SimpleStreamTransducer
import fpinscala.testing.{ Prop, Gen }

import scala.language.higherKinds
import scalaz.Applicative


object Main extends App {

  object ZApplicativeBuilder {
    import scalaz.Scalaz._
    (9.some |@| 1.some)(_ + _)
    (Option(9) |@| Option(1))(_ + _)

    // Applicative.sequence
    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
      case Nil     => List.empty[A].point[F]
      case x :: xs => (x |@| sequenceA(xs))(_ :: _)
    }
  }

  def ZCompose: Unit = {
    val f = (_: Int) + 1
    val g = (_: Int) * 100

    import scalaz.Scalaz._
    println((f >>> g)(2))
    println((f <<< g)(2))
  }

  def MyCompose: Unit = {
    val f = (_: Int) + 1
    val g = (_: Int) * 100
    import fpinscala.category.instance.ComposeInstance._
    function1Instance.composeSyntax.ToComposeOps(f).<<<(g)(2)
    import fpinscala.category.syntax.ToComposeOps._
    println((f >>> g)(2))
    println((f <<< g)(2))
  }

  def ZSplit: Unit = {
    val f = (_: Int) + 1
    val g = (_: Int) * 100

    import scalaz.Scalaz._
    println((f -*- g)(1, 2))
  }

  def MySplit: Unit = {
    val f = (_: Int) + 1
    val g = (_: Int) * 100
    import fpinscala.category.instance.SplitInstance._
    function1Instance.splitSyntax.ToSplitOps(f).-*-(g)(1, 2)
    import fpinscala.category.syntax.ToSplitOps._
    println((f -*- g)(1, 2))
  }

  ZCompose
  MyCompose

  ZSplit
  MySplit

}
