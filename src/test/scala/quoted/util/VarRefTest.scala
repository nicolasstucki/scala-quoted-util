package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Toolbox._

class VarRefTest {

  @Test def varRef0: Unit = {
    val block = VarRef(4.toExpr)((x, `x = `) => '{ ~`x = `(3.toExpr); ~x })
    assertEquals(
      """{
        |  var x: Int = 4
        |  x = 3
        |  x
        |}""".stripMargin, block.show)
  }

  @Test def varRef1: Unit = {
    val block = VarRef('(7)) {
       (x, `x = `) => '{
         while(0 < ~x)
           ~`x = `('(~x - 1))
         ~x
       }
     }
    assertEquals(
      """{
        |  var x: Int = 7
        |  def while$(): Unit = 
        |    if 0.<(x) then 
        |      {
        |        x = x.-(1)
        |        while$()
        |      }
        |     else ()
        |  while$()
        |  x
        |}""".stripMargin, block.show)
  }

}
