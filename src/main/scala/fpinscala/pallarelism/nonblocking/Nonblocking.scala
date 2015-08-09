package fpinscala.pallarelism.nonblocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import fpinscala.either.{MyRight, MyLeft, MyEither}
import fpinscala.option.{MyNone, MyOption, MySome}

object Nonblocking {

  trait Future[+A] {
    // APIとしての純粋性を保つため
    private[pallarelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      // スレッドセーフなミュータブル参照
      val ref = new AtomicReference[A]
      // 特定回数呼び出されるまでスレッドを待機させることができる
      val latch = new CountDownLatch(1)
      // {}は「a: A => Unit」
      p(es) {
        a =>
          ref.set(a)
          latch.countDown()
      }
      latch.await()
      ref.get()
    }

    def unit[A](a: A): Par[A] = {
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }
    }

    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      private[pallarelism] def apply(k: (A) => Unit): Unit = f(k)
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = {
      es.submit(new Callable[Unit] {
        def call = r
      })
    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: MyOption[A] = MyNone
          var br: MyOption[B] = MyNone
          val combiner = Actor[MyEither[A, B]](es) {
            case MyLeft(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = MySome(a)
            case MyRight(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = MySome(b)
          }
          p(es)(a => combiner ! MyLeft(a))
          p2(es)(b => combiner ! MyRight(b))
        }
      }

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    /*
      def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = fork {
        val pars: List[Par[List[A]]] = ps.map(asyncF(a => if (f(a)) List(a) else List()))
        map(sequence(pars))(_.flatten)
      }*/

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

    // note: Par[List[A]]を明示的に渡してあげないと、map2がPar[B]の型を解釈できない
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }

}