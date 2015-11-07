package fpinscala.kleisli

import scala.language.higherKinds

final case class Kleisli[M[_], A, B](run: A => M[B])

object Kleisli {

  // Kleisli factory for Function1
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] =
    Kleisli(f)


}