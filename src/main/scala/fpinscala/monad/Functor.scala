package fpinscala.monad

import fpinscala.collection.list.MyList
import fpinscala.either.{MyEither, MyLeft, MyRight}
import fpinscala.streamingio.SimpleStreamTransducer.Process

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(f => f._1), map(fab)(f => f._2))

  def codistribute[A, B](e: MyEither[F[A], F[B]]): F[MyEither[A, B]] = e match {
    case MyLeft(fa) => map(fa)(MyLeft(_))
    case MyRight(fb) => map(fb)(MyRight(_))
  }

}

object Functor {

  val listFunction: Functor[MyList] = new Functor[MyList] {
    override def map[A, B](as: MyList[A])(f: (A) => B): MyList[B] =
      as.map(f)
  }

  def processFunctor[I] = new Functor[({type lambda[o] = Process[I, o]})#lambda] {
    def map[A, B](fa: Process[I, A])(f: (A) => B): Process[I, B] =
      fa.map(f)
  }
}
