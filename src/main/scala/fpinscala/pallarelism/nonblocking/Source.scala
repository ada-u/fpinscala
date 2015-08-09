package fpinscala.pallarelism.nonblocking

import fpinscala.either.MyEither

trait Source {

  def readBytes(numBytes: Int, callback: MyEither[Throwable, List[Byte]] => Unit): Unit

}
