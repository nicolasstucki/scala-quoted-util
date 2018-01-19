package scala.quoted
package util.liftables

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class TuplesTest {

  @Test def testLifing: Unit = {
    import Tuples._
    val t1: Expr[Tuple1[Int]] = Tuple1(5)
    val t2: Expr[(Int, Int)] = (1, 2)
    val t3: Expr[(Int, Int, Int)] = (1, 2, 3)
    val t4: Expr[(Int, Int, Int, Int)] = (1, 2, 3, 4)

    // FIXME
    // t1.run


  }

}