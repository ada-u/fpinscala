package fpinscala.pallarelism.blocking

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration._

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (ex: ExecutorService) => UnitFuture(a)

  // 派生コンビネータ
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a, _) => f(a))

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(pa).get
      run(es)(choices(k))
    }


  /**
   * 7.5 map2の初期実装
   *
   * 問題点: Futureのタイムアウト規則に従わない
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (ex: ExecutorService) => {
      val af = a(ex)
      val bf = b(ex)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
    p(e).get == p2(e).get
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] = ps.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  // note: Par[List[A]]を明示的に渡してあげないと、map2がPar[B]の型を解釈できない
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((x, xs) => map2(x, xs)(_ :: _))

  /**
   * 7.5 最も単純なforkの実装
   *
   * 問題点: 外側のCallableが、内側のタスク終了までブロックする。
   * スレッド1つで充分なのに、２つ分のスレッドを消費している。
   */
  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  //
  def delay[A](a: => Par[A]): Par[A] =
    es => a(es)

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A])

}