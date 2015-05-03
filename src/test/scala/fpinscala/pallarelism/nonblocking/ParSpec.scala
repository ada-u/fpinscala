package fpinscala.pallarelism.nonblocking

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.pallarelism.blocking.Par
import fpinscala.testing.Prop
import fpinscala.testing.Prop._
import org.scalatest.{FlatSpec, DiagrammedAssertions}

class ParSpec extends FlatSpec with DiagrammedAssertions {

  val ES: ExecutorService = Executors.newCachedThreadPool

  "Par" should "satisfy Identity Laws" in {
    Prop.run {
      check {
        equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
      }
    }
  }

}
