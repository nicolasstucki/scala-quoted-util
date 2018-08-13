package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.UnrolledExpr._
import scala.quoted.util.Let._

import org.junit.Test
import org.junit.Assert._

class VarTest {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  @Test def varRef0: Unit = {
    val block = `var`(4.toExpr)(x => '{ ~x.update(3.toExpr); ~x.ref })
    assertEquals(
      """{
        |  var x: scala.Int = 4
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
        |  var x: scala.Int = 7
        |  while (0.<(x)) x = x.-(1)
        |  x
        |}""".stripMargin, block.show)
  }

}
