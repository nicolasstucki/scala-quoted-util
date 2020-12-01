package scala.quoted
package util

import scala.quoted.util.Let._
import scala.quoted.staging._

import org.junit.Test
import org.junit.Assert._

class ConstTest {

  implicit val toolbox: Toolbox = Toolbox.make(getClass.getClassLoader)

  @Test def testBoolean: Unit = withQuotes {
    assertEquals(Some(true), Const.unapply('{true}))
    assertEquals(Some(false), Const.unapply('{false}))
  }

  @Test def testByte: Unit = withQuotes {
    assertEquals(Some(0: Byte), Const.unapply('{0: Byte}))
    assertEquals(Some(1: Byte), Const.unapply('{1: Byte}))
  }

  @Test def testShort: Unit = withQuotes {
    assertEquals(Some(0), Const.unapply('{0}))
    assertEquals(Some(1), Const.unapply('{1}))
    assertEquals(Some(100), Const.unapply('{100}))
  }

  @Test def testInt: Unit = withQuotes {
    assertEquals(Some(0), Const.unapply('{0}))
    assertEquals(Some(1), Const.unapply('{1}))
    assertEquals(Some(100), Const.unapply('{100}))
  }

  @Test def testLong: Unit = withQuotes {
    assertEquals(Some(0L), Const.unapply('{0L}))
    assertEquals(Some(1L), Const.unapply('{1L}))
    assertEquals(Some(100L), Const.unapply('{100L}))
  }

  @Test def testFloat: Unit = withQuotes {
    assertEquals(Some(0f), Const.unapply('{0f}))
    assertEquals(Some(1f), Const.unapply('{1f}))
    assertEquals(Some(100f), Const.unapply('{100f}))
  }

  @Test def testDouble: Unit = withQuotes {
    assertEquals(Some(0d), Const.unapply('{0d}))
    assertEquals(Some(1d), Const.unapply('{1d}))
    assertEquals(Some(100d), Const.unapply('{100d}))
  }

  @Test def testString: Unit = withQuotes {
    assertEquals(Some(""), Const.unapply('{""}))
    assertEquals(Some("abc"), Const.unapply('{"abc"}))
  }

  @Test def testNull: Unit = withQuotes {
    assertEquals(None, Const.unapply('{null}))
  }

  @Test def testUnit: Unit = withQuotes {
    assertEquals(None, Const.unapply('{()}))
  }

  @Test def testClassOf: Unit = withQuotes {
    assertEquals(None, Const.unapply('{classOf[Int]}))
  }

  @Test def testOthers: Unit = withQuotes {
    assertEquals(None, Const.unapply('{new Object}))
    assertEquals(None, Const.unapply('{val a: Int = 3; a}))
  }
}
