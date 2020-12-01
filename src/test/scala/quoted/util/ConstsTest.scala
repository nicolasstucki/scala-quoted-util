package scala.quoted
package util

import scala.quoted.util.Let._
import scala.quoted.staging._

import org.junit.Test
import org.junit.Assert._

class ConstsTest {

  implicit val toolbox: Toolbox = Toolbox.make(getClass.getClassLoader)

  @Test def testEmpty: Unit = withQuotes {
    assertEquals(Some(Seq()), Consts.unapply(Seq()))
  }

  @Test def testInts: Unit = withQuotes {
    assertEquals(Some(Seq(1)), Consts.unapply(Seq('{1})))
    assertEquals(Some(Seq(1, 2, 3)), Consts.unapply(Seq('{1}, '{2}, '{3})))
    
    assertEquals(None, Consts.unapply(Seq('{val a: Int = 1; a}, '{2}, '{3})))
    assertEquals(None, Consts.unapply(Seq('{1}, '{val a: Int = 2; a}, '{3})))
    assertEquals(None, Consts.unapply(Seq('{1}, '{2}, '{val a: Int = 3; a})))
  }

}
