package fpinscala.benchmark

object Benchmark {

  def run(action: => Unit, noTimes: Int, multiplier: Int = 1): Double = {
    import java.util.concurrent.TimeUnit
    val runtime = Runtime.getRuntime()
    val result = (1 to noTimes + 1) map { _ =>
      val startTime = System.nanoTime
      var i = 0
      while (i < multiplier) {
        action
        i += 1
      }
      val stopTime = System.nanoTime
      runtime.gc()

      TimeUnit.MICROSECONDS.convert(stopTime - startTime,
        TimeUnit.NANOSECONDS).asInstanceOf[Double]
    }
    result.sum / noTimes
  }
}