import fpinscala.localeffects.{RunnableST, ST, STRef}

import scala.language.higherKinds


object Main extends App {

  /*
    object ZApplicativeBuilder {

      import scalaz.Scalaz._

      (9.some |@| 1.some) (_ + _)
      (Option(9) |@| Option(1)) (_ + _)

      // Applicative.sequence
      def sequenceA[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = list match {
        case Nil => List.empty[A].point[F]
        case x :: xs => (x |@| sequenceA(xs)) (_ :: _)
      }
    }

    def ZCompose: Unit = {
      val f = (_: Int) + 1
      val g = (_: Int) * 100

      import scalaz.Scalaz._
      println((f >>> g) (2))
      println((f <<< g) (2))
    }

    def MyCompose: Unit = {
      val f = (_: Int) + 1
      val g = (_: Int) * 100
      import fpinscala.category.instance.ComposeInstance._
      function1Instance.composeSyntax.ToComposeOps(f).<<<(g)(2)
      import fpinscala.category.syntax.ToComposeOps._
      println((f >>> g) (2))
      println((f <<< g) (2))
    }

    def ZSplit: Unit = {
      val f = (_: Int) + 1
      val g = (_: Int) * 100

      import scalaz.Scalaz._
      println((f -*- g) (1, 2))
      function1Instance.categorySyntax.ToCategoryOps(f)
    }

    def MySplit: Unit = {
      val f = (_: Int) + 1
      val g = (_: Int) * 100
      import fpinscala.category.instance.SplitInstance._
      function1Instance.splitSyntax.ToSplitOps(f).-*-(g)(1, 2)
      import fpinscala.category.syntax.ToSplitOps._
      println((f -*- g) (1, 2))
    }

    def MyCategory: Unit = {
      val f = (_: Int) + 1
      val g = (_: Int) * 100

      import fpinscala.category.instance.CategoryInstance._
      function1Instance.categorySyntax.ToCategoryOps(f).<<<(g)(2)
      import fpinscala.category.syntax.ToCategoryOps._
      println((f >>> g) (2))
      println((f <<< g) (2))
    }


    ZCompose
    MyCompose
    MyCategory

    ZSplit
    MySplit

    println("---------------------")
    println(MyStream(1, 2, 3, 4).takeWhile(_ < 2).toList)
    println(MyStream(1,2,3,4).headOption)

    val es = Executors.newFixedThreadPool(1)

  */


  /*val p = for {
    r1 <- STRef(1)
    r2 <- STRef(2)
    x <- r1.read
    y <- r2.read
    _ <- r1.write(y + 1)
    _ <- r2.write(x + 1)
    a <- r1.read
    b <- r2.read
  } yield (a, b)*/

  val p2 = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  println(ST.runST(p2))

  /**
    * trait RunnableST[A] {
    *  def apply[S]: ST[S, A]
    * }
    */
  // unrunnable
  /*val p3 = new RunnableST[STRef[Nothing, Int]] {
    def apply[S] = {
      val unrunnable: ST[S, STRef[S, Int]] = STRef.apply[S, Int](1)
      unrunnable
    }
  }*/

  val p4: ST[Nothing, STRef[Nothing, Int]] = STRef(1)


  //Trampoline.runIO

  object Overflow {

    import fpinscala.io.IO

    def run = {
      val p: IO[Nothing] = IO.forever(PrintLine("Still going..."))

      // foreverの中でPrintLine#flatMap -> (traitの)IO#flatMap
      p.run
    }

    def PrintLine(msg: String): IO[Unit] = IO {
      println(msg)
    }
  }

  //Trampoline.runIO
  //Overflow.run

  //TailRec.trampoline()

  //LocalEffects.quickSort(List(9, 7, 5, 8, 6, 4, 3, 1, 2))

  object Trampoline {

    import fpinscala.io.trampoline._

    def PrintLine(msg: String): IO[Unit] =
      Suspend(() => println(msg))

    @annotation.tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }

    def runIO = {
      // FlatMap(Suspend(() => println(s)),
      //    _ => FlatMap(Suspend(() => println(s)),
      //         _ => FlatMap(Suspend(() => println(s)),
      //              _ => FlatMap(...)))
      val p = IO.forever(PrintLine("Still going..."))
      run(p)
    }
  }

  object TailRec {

    import fpinscala.io.trampoline._

    def sof = {
      val f: (Int) => Int = (x: Int) => x
      val g: (Int) => Int = List.fill(100000)(f).foldLeft(f)(_ compose _)

      // スタックpushしまくって死ぬ
      g(42)
    }

    @annotation.tailrec
    def debugRun[A](io: IO[A]): A = io match {
      case Return(a) =>
        println(s"Return($a))")
        a
      case Suspend(r) =>
        println("Suspend)")
        r()
      case FlatMap(x, f) =>
        print("FlatMap(")
        x match {
          case Return(a) =>
            print(s"Return($a), ")
            debugRun(f(a))
          case Suspend(r) =>
            print("Suspend, ")
            debugRun(f(r()))
          case FlatMap(y, g) =>
            print("FlatMap(")
            debugRun(y flatMap (a => g(a) flatMap f))
        }
    }

    def trampoline() = {
      val f: (Int) => IO[Int] = (x: Int) => Return(x)
      // FlatMap(Suspend, _ => FlatMap(Return, x => FlatMap(FlatMap(Suspend, _ => FlatMap(Return, x => FlatMap())))))...
      val g: (Int) => IO[Int] = List.fill(10)(f).foldLeft[Int => IO[Int]](f) {
        (a: Int => IO[Int], b: Int => IO[Int]) =>
          x =>
            // 関数を呼ぶ前にSuspendを挟んで必ずrunに処理を戻す
            Suspend(() => ()).flatMap {
              _ =>
                b(x).flatMap(a)
            }
      }
      println(debugRun(g(42)))
    }
  }


}
