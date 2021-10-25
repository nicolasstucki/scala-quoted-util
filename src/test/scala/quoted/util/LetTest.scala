package scala.quoted
package util

import scala.quoted.util.Let._
import scala.quoted.staging._

import org.junit.Test
import org.junit.Assert._

class VarTest {

  given Compiler = Compiler.make(getClass.getClassLoader)

  @Test def varRef0: Unit = withQuotes {
    val block = Let.`var`(Expr(4))(x => '{ ${x.update(Expr(3))}; ${x.ref} })
    assertEquals(
      """{
        |  var x: scala.Int = 4
        |  x = 3
        |  x
        |}""".stripMargin, block.show)
  }

  @Test def varRef1: Unit = withQuotes {
    val block = `var`('{7}) {
      x => '{
         while(0 < ${x.ref})
           ${x.update('{${x.ref} - 1})}
         ${x.ref}
       }
     }
    assertEquals(
      """{
        |  var x: scala.Int = 7
        |  while (0.<(x)) x = x.-(1)
        |  x
        |}""".stripMargin, block.show)
  }

  @Test def valRef: Unit = withQuotes {
    val block = Let.`val`(Expr(4))(x => '{ ${x.ref} + ${x.ref} })
    assertEquals(
      """{
        |  val x: scala.Int = 4
        |  x.+(x)
        |}""".stripMargin, block.show)
  }

    @Test def staticRef: Unit = withQuotes {
    val block = Let.static(4)(x => '{ ${x.ref} + ${Expr(x.value)} })
    assertEquals(
      """{
        |  val x: scala.Int = 4
        |  x.+(4)
        |}""".stripMargin, block.show)
  }

}
