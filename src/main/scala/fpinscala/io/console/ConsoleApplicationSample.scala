package fpinscala.io.console

import fpinscala.io.free.Free

object ConsoleApplicationSample {

  val app: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console")
    ln <- Console.readLn
  } yield ln
  
}
