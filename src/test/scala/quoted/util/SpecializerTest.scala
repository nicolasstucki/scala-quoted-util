package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.UnrolledExpr._
import scala.quoted.util.Specializer._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Toolbox._

class SpecializerTest {

  @Test def specializeTest: Unit = {
    def typedToString[T: Type](a: Expr[T]): Expr[String] = specialize(a) {
      case UnitExpr(a) => '{ s"(${~a}: Unit)" }
      case BooleanExpr(a) => '{ s"(${~a}: Boolean)" }
      case ByteExpr(a) => '{ s"(${~a}: Byte)" }
      case CharExpr(a) => '{ s"(${~a}: Char)" }
      case ShortExpr(a) => '{ s"(${~a}: Short)" }
      case IntExpr(a) => '{ s"(${~a}: Int)" }
      case LongExpr(a) => '{ s"(${~a}: Long)" }
      case FloatExpr(a) => '{ s"(${~a}: Float)" }
      case DoubleExpr(a) => '{ s"(${~a}: Double)" }
      case _ => '{ s"(${~a}: T)" }
    }

    // assertEquals("", typedToString[Unit]('()).run) // FIXME
    assertEquals("(true: Boolean)", typedToString('(true)).run)
    assertEquals("(1: Byte)", typedToString('(1.toByte)).run)
    assertEquals("(F: Char)", typedToString('(70.toChar)).run)
    assertEquals("(1: Short)", typedToString('(1.toShort)).run)
    assertEquals("(1: Int)", typedToString('(1)).run)
    assertEquals("(1: Long)", typedToString('(1L)).run)
    assertEquals("(1.0: Float)", typedToString('(1f)).run)
    assertEquals("(1.0: Double)", typedToString('(1d)).run)
    assertEquals("(abc: T)", typedToString('("abc")).run)
  }

  @Test def specializeListTest: Unit = {
    def specializedEquals[T: Type](a: Expr[T], b: Expr[T]): Expr[Boolean] = specialize(a :: b :: Nil) {
      case IntExprList(a :: b :: Nil) => '{ ~a == ~b + 3 - 3 }
      case _ => '{ ~a == ~b }
    }

    assertEquals("1.==(2.+(3).-(3))", specializedEquals('(1), '(2)).show)
    assertEquals("\"abc\".==(\"def\")", specializedEquals('("abc"), '("def")).show)
  }

}