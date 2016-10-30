package fpinscala.state

import fpinscala.collection.list.MyList
import fpinscala.collection.stream.MyStream
import fpinscala.option.{ MySome, MyOption, MyNone }
import fpinscala.state.VendingMachine.{ Input, Machine, Turn, Coin }
import org.scalatest.{ DiagrammedAssertions, FlatSpec, FunSuite }

class VendingMachineSpec extends FlatSpec with DiagrammedAssertions {

  val buy: Int => MyList[Input] = (n: Int) => MyList.fill(n)(MyList(Coin, Turn)).flatMap(identity)

  it should "入力であるMachineに硬貨が10枚、スナックが5個入っていて、合計で4個のスナックが正常に購入された場合、出力は(14, 1)になるはずだ" in {
    val machine = Machine(locked = true, candies = 5, coins = 10)

    val result = VendingMachine.simulateMachine(buy(4)).run(machine)
    assert((result._1._1, result._1._2) == (14, 1))
  }

  ignore should "ロックされた状態の自動販売機に硬貨を投入すると、スナックが残っている場合はロックが解除される" in {

  }

  ignore should "ロックが解除された状態の自動販売機のハンドルを回すと、スナックが出てきてロックがかかる" in {

  }

  ignore should "ロックされた状態でハンドルを回したり、ロックが解除された状態で硬貨を投入したりしても何も起こらない" in {

  }

  ignore should "スナックが売り切れた自動販売機は入力をすべて無視する" in {

  }


}
