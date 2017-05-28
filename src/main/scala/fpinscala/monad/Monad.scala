package fpinscala.monad

import fpinscala.applicative.Applicative
import fpinscala.collection.list.MyList
import fpinscala.collection.stream.MyStream
import fpinscala.either.{MyEither, MyLeft, MyRight}
import fpinscala.option.{MyOption, MySome}
import fpinscala.pallarelism.blocking.Par
import fpinscala.pallarelism.blocking.Par.Par
import fpinscala.pallarelism.nonblocking.Nonblocking.{Par => NBPar}
import fpinscala.parser.Parsers
import fpinscala.state.State
import fpinscala.streamingio.SimpleStreamTransducer._
import fpinscala.testing.Gen
import fpinscala.traverse.Traverse

import scala.language.{higherKinds, implicitConversions}

trait Monad[F[_]] extends Applicative[F] {

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)()

  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    a.flatMap(_ => t)
  }

  def filterM[A](la: MyList[A])(f: A => F[Boolean]): F[MyList[A]] =
    la.foldRight(unit(MyList.empty[A]))((x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x)
    )

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  implicit def toMonadic[A](a: F[A]): Monadic[F, A] =
    new Monadic[F, A] {
      val F = Monad.this

      def get = a
    }
}

trait Monadic[F[_], A] {

  val F: Monad[F]
  private val a = get

  def get: F[A]

  def map[B](f: A => B): F[B] = F.map(a)(f)

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
}

object Monad {

  val genMonad = new Monad[Gen] {

    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)

  }

  val parMonad = new Monad[Par] {

    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = ???

  }

  val nbParMonad = new Monad[NBPar] {

    override def unit[A](a: => A): NBPar[A] = NBPar.unit(a)

    override def flatMap[A, B](ma: NBPar[A])(f: A => NBPar[B]): NBPar[B] = ma.flatMap(f)

  }

  val listMonad = new Monad[MyList] {

    override def unit[A](a: => A): MyList[A] = MyList(a)

    override def flatMap[A, B](ma: MyList[A])(f: A => MyList[B]): MyList[B] = ma.flatMap(f)

  }

  val streamMonad = new Monad[MyStream] {

    override def unit[A](a: => A): MyStream[A] = MyStream(a)

    override def flatMap[A, B](ma: MyStream[A])(f: (A) => MyStream[B]): MyStream[B] = ma.flatMap(f)

  }

  val optionMonad = new Monad[MyOption] {

    override def unit[A](a: => A): MyOption[A] = MySome(a)

    override def flatMap[A, B](ma: MyOption[A])(f: (A) => MyOption[B]): MyOption[B] = ma.flatMap(f)

  }
  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a

    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  def parserMonad[P[+ _]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {

    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)

  }

  def eitherMonad[E] = new Monad[({type f[x] = MyEither[E, x]})#f] {

    override def flatMap[A, B](ma: MyEither[E, A])(f: (A) => MyEither[E, B]): MyEither[E, B] = ma match {
      case MyRight(a) => f(a)
      case left@MyLeft(_) => left
    }

    override def unit[A](a: => A): MyEither[E, A] =
      MyRight(a)
  }

  implicit def processMonad[I] =
    new Monad[({type λ[x] = Process[I, x]})#λ] {
      def unit[A](a: => A): Process[I, A] =
        Emit(a)

      def flatMap[A, B](ma: Process[I, A])(f: (A) => Process[I, B]): Process[I, B] =
        ma.flatMap(f)

    }

  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {

    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

    override def flatMap[A, B](gha: G[H[A]])(f: A => G[H[B]]): G[H[B]] = {
      val result: G[H[B]] = G.flatMap(gha) {
        (ha: H[A]) =>
          val ghb: G[H[B]] = G.map {
            val ghhb: G[H[H[B]]] = T.traverse(ha)(f)(G) // HHA -> HHB -> GHHB
            ghhb
          }(H.join)
          ghb
      }
      result
    }
  }

}

trait MonadCatch[F[_]] extends Monad[F] {

  def attempt[A](a: F[A]): F[Either[Throwable, A]]

  def fail[A](t: Throwable): F[A]

}