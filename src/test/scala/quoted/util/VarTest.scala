package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._
import scala.quoted.util.Let._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Toolbox._

class VarTest {

  @Test def varRef0: Unit = {
    val block = `var`(4.toExpr)(x => '{ ~x.update(3.toExpr); ~x.ref })
    assertEquals(
      """{
        |  var x: Int = 4
        |  x = 3
        |  x
        |}""".stripMargin, block.show)
  }

  @Test def varRef1: Unit = {
    val block = `var`('(7)) {
      x => '{
         while(0 < ~x.ref)
           ~x.update('(~x.ref - 1))
         ~x.ref
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
