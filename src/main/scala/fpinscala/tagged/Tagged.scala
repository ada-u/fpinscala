package fpinscala.tagged

import scalaz.{ Tag, @@ }

object Tagged {


  sealed trait KiloGram

  // @@[A, KiloGram]
  def kiloGram[A](a: A): A @@ KiloGram =
    Tag[A, KiloGram](a)

  val mass = kiloGram(100)

}
