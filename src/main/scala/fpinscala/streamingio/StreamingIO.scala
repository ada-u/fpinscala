package fpinscala.streamingio

import java.util.concurrent.Executors

import fpinscala.io.console.ConsoleApplicationSample
import fpinscala.io.free.Free
import fpinscala.io.free.Free.IO
import fpinscala.pallarelism.nonblocking.Nonblocking.Par

import scala.io.Source

class StreamingIO {

}

object StreamingIO {

}

object NonComposableProcessingWithIO {

  def lineGt40k_1(filename: String): IO[Boolean] = IO {
    val src = Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    } finally src.close()
  }

  def lineGt40k_2(filename: String): IO[Boolean] =
    lines(filename).map(_.zipWithIndex.exists(_._2 + 1 >= 40000))


  def lines(filename: String): IO[Stream[String]] = IO {
    val src = Source.fromFile(filename)
    src.getLines().toStream.append {
      src.close()
      Stream.empty
    }
  }

  def runLineGt40k: Boolean = {
    val app: IO[Boolean] = NonComposableProcessingWithIO.lineGt40k_2("/Users/ada-u/.tmux.conf")
    Par.run(Executors.newCachedThreadPool)(Free.run(app)(ConsoleApplicationSample.par0Monad))
  }

}
