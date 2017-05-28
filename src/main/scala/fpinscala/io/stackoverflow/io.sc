import fpinscala.io.IO

def PrintLine(msg: String): IO[Unit] = IO {
  println(msg)
}
val p = IO.forever(PrintLine("Still going..."))

p.run