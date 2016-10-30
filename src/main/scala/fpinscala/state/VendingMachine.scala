package fpinscala.state

import java.util
import java.util.concurrent
import java.util.concurrent.{ TimeUnit, Callable, ExecutorService }

import fpinscala.collection.list.MyList
import fpinscala.pallarelism.nonblocking.Nonblocking

object VendingMachine {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: MyList[Input]): State[Machine, (Int, Int)] = {

    def process: Input => Machine => Machine =
      input => machine => (input, machine) match {
        case (_, Machine(_, 0, _)) =>
          machine
        case (Coin, Machine(true, candies, coins)) =>
          Machine(locked = false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) =>
          Machine(locked = true, candies - 1, coins)
        case (Coin, Machine(false, _, _)) | (Turn, Machine(true, _, _)) =>
          machine
      }

    val states: MyList[State[Machine, (Int, Int)]] = inputs.map {
      input => State {
        machine =>
          val result = process(input)(machine)
          ((result.candies, result.coins), result)
      }
    }

    State.sequence(states).get.map(last => (last.candies, last.coins))
  }


  import Nonblocking._

  Par.unit(new ExecutorService {override def shutdown(): Unit = ???

    override def isTerminated: Boolean = ???

    override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = ???

    override def shutdownNow(): util.List[Runnable] = ???

    override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]]): util.List[concurrent.Future[T]] = ???

    override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): util.List[concurrent.Future[T]] = ???

    override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]]): T = ???

    override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T = ???

    override def isShutdown: Boolean = ???

    override def submit[T](task: Callable[T]): concurrent.Future[T] = ???

    override def submit[T](task: Runnable, result: T): concurrent.Future[T] = ???

    override def submit(task: Runnable): concurrent.Future[_] = ???

    override def execute(command: Runnable): Unit = ???
  })

}
