package scala.quoted.math

import scala.language.implicitConversions

import scala.quoted._

object NumericExpr {
  trait ExtraImplicits {
    /** These implicits create conversions from a value for which an implicit Numeric
     *  exists to the inner class which creates infix operations.  Once imported, you
     *  can write methods as follows:
     *  {{{
     *  def plus[T: Numeric](x: T, y: T) = x + y
     *  }}}
     */
    implicit def infixNumericOps[T](x: Expr[T])(implicit num: NumericExpr[T]): NumericExpr[T]#Ops = new num.Ops(x)
  }
  object Implicits extends ExtraImplicits { }

  trait BigIntIsIntegral extends Integral[BigInt] {
    def plus(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = '(~x + ~y)
    def minus(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = '(~x - ~y)
    def times(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = '(~x * ~y)
    def quot(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = '(~x / ~y)
    def rem(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = '(~x % ~y)
    def negate(x: Expr[BigInt]): Expr[BigInt] = '(-(~x))
    def fromInt(x: Int): Expr[BigInt] = '(BigInt(~x.toExpr))
    def toInt(x: Expr[BigInt]): Expr[BigInt] = x
    def toLong(x: Expr[BigInt]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[BigInt]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[BigInt]): Expr[Double] = '((~x).toDouble)
  }
  implicit object BigIntIsIntegral extends BigIntIsIntegral with Ordering.BigIntOrdering

  trait IntExprIsIntegral extends Integral[Int] {
    def plus(x: Expr[Int], y: Expr[Int]): Expr[Int] = '(~x + ~y)
    def minus(x: Expr[Int], y: Expr[Int]): Expr[Int] = '(~x - ~y)
    def times(x: Expr[Int], y: Expr[Int]): Expr[Int] = '(~x * ~y)
    def quot(x: Expr[Int], y: Expr[Int]): Expr[Int] = '(~x / ~y)
    def rem(x: Expr[Int], y: Expr[Int]): Expr[Int] = '(~x % ~y)
    def negate(x: Expr[Int]): Expr[Int] = '(-(~x))
    def fromInt(x: Int): Expr[Int] = x.toExpr
    def toInt(x: Expr[Int]): Expr[Int] = x
    def toLong(x: Expr[Int]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Int]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Int]): Expr[Double] = '((~x).toDouble)
  }
  implicit object IntExprIsIntegral extends IntExprIsIntegral with Ordering.IntOrdering

  trait ShortExprIsIntegral extends Integral[Short] {
    def plus(x: Expr[Short], y: Expr[Short]): Expr[Short] = '((~x + ~y).toShort)
    def minus(x: Expr[Short], y: Expr[Short]): Expr[Short] = '((~x - ~y).toShort)
    def times(x: Expr[Short], y: Expr[Short]): Expr[Short] = '((~x * ~y).toShort)
    def quot(x: Expr[Short], y: Expr[Short]): Expr[Short] = '((~x / ~y).toShort)
    def rem(x: Expr[Short], y: Expr[Short]): Expr[Short] = '((~x % ~y).toShort)
    def negate(x: Expr[Short]): Expr[Short] = '((-(~x)).toShort)
    def fromInt(x: Int): Expr[Short] = '((~x.toExpr).toShort)
    def toInt(x: Expr[Short]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Short]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Short]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Short]): Expr[Double] = '((~x).toDouble)
  }
  implicit object ShortExprIsIntegral extends ShortExprIsIntegral with Ordering.ShortOrdering

  trait ByteIsIntegral extends Integral[Byte] {
    def plus(x: Expr[Byte], y: Expr[Byte]): Expr[Byte] = '((~x + ~y).toByte)
    def minus(x: Expr[Byte], y: Expr[Byte]): Expr[Byte] = '((~x - ~y).toByte)
    def times(x: Expr[Byte], y: Expr[Byte]): Expr[Byte] = '((~x * ~y).toByte)
    def quot(x: Expr[Byte], y: Expr[Byte]): Expr[Byte] = '((~x / ~y).toByte)
    def rem(x: Expr[Byte], y: Expr[Byte]): Expr[Byte] = '((~x % ~y).toByte)
    def negate(x: Expr[Byte]): Expr[Byte] = '((-(~x)).toByte)
    def fromInt(x: Int): Expr[Byte] = '((~x.toExpr).toByte)
    def toInt(x: Expr[Byte]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Byte]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Byte]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Byte]): Expr[Double] = '((~x).toDouble)
  }
  implicit object ByteIsIntegral extends ByteIsIntegral with Ordering.ByteOrdering

  trait CharIsIntegral extends Integral[Char] {
    def plus(x: Expr[Char], y: Expr[Char]): Expr[Char] = '((~x + ~y).toChar)
    def minus(x: Expr[Char], y: Expr[Char]): Expr[Char] = '((~x - ~y).toChar)
    def times(x: Expr[Char], y: Expr[Char]): Expr[Char] = '((~x * ~y).toChar)
    def quot(x: Expr[Char], y: Expr[Char]): Expr[Char] = '((~x / ~y).toChar)
    def rem(x: Expr[Char], y: Expr[Char]): Expr[Char] = '((~x % ~y).toChar)
    def negate(x: Expr[Char]): Expr[Char] = '((-(~x)).toChar)
    def fromInt(x: Int): Expr[Char] = '((~x.toExpr).toChar)
    def toInt(x: Expr[Char]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Char]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Char]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Char]): Expr[Double] = '((~x).toDouble)
  }
  implicit object CharIsIntegral extends CharIsIntegral with Ordering.CharOrdering

  trait LongExprIsIntegral extends Integral[Long] {
    def plus(x: Expr[Long], y: Expr[Long]): Expr[Long] = '(~x + ~y)
    def minus(x: Expr[Long], y: Expr[Long]): Expr[Long] = '(~x - ~y)
    def times(x: Expr[Long], y: Expr[Long]): Expr[Long] = '(~x * ~y)
    def quot(x: Expr[Long], y: Expr[Long]): Expr[Long] = '(~x / ~y)
    def rem(x: Expr[Long], y: Expr[Long]): Expr[Long] = '(~x % ~y)
    def negate(x: Expr[Long]): Expr[Long] = '(-(~x))
    def fromInt(x: Int): Expr[Long] = x.toLong.toExpr
    def toInt(x: Expr[Long]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Long]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Long]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Long]): Expr[Double] = '((~x).toDouble)
  }
  implicit object LongExprIsIntegral extends LongExprIsIntegral with Ordering.LongOrdering

  trait FloatExprIsConflicted extends NumericExpr[Float] {
    def plus(x: Expr[Float], y: Expr[Float]): Expr[Float] = '(~x + ~y)
    def minus(x: Expr[Float], y: Expr[Float]): Expr[Float] = '(~x - ~y)
    def times(x: Expr[Float], y: Expr[Float]): Expr[Float] = '(~x * ~y)
    def negate(x: Expr[Float]): Expr[Float] = '(-(~x))
    def fromInt(x: Int): Expr[Float] = x.toFloat.toExpr
    def toInt(x: Expr[Float]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Float]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Float]): Expr[Float] = x
    def toDouble(x: Expr[Float]): Expr[Double] = '((~x).toDouble)
    // logic in Numeric base trait mishandles abs(-0.0f)
    // override def abs(x: Float): Float = math.abs(x)
  }
  trait FloatExprIsFractional extends FloatExprIsConflicted with Fractional[Float] {
    def div(x: Float, y: Float): Float = x / y
  }
  trait FloatExprAsIfIntegral extends FloatExprIsConflicted with Integral[Float] {
    def quot(x: Float, y: Float): Float = (BigDecimal(x) quot BigDecimal(y)).floatValue
    def rem(x: Float, y: Float): Float = (BigDecimal(x) remainder BigDecimal(y)).floatValue
  }
  implicit object FloatExprIsFractional extends FloatExprIsFractional with Ordering.FloatOrdering
  object FloatAsIfIntegral extends FloatExprAsIfIntegral with Ordering.FloatOrdering {
  }

  trait DoubleExprIsConflicted extends NumericExpr[Double] {
    def plus(x: Expr[Double], y: Expr[Double]): Expr[Double] = '(~x + ~y)
    def minus(x: Expr[Double], y: Expr[Double]): Expr[Double] = '(~x - ~y)
    def times(x: Expr[Double], y: Expr[Double]): Expr[Double] = '(~x * ~y)
    def negate(x: Expr[Double]): Expr[Double] = '(-(~x))
    def fromInt(x: Int): Expr[Double] = x.toDouble.toExpr
    def toInt(x: Expr[Double]): Expr[Int] = '((~x).toInt)
    def toLong(x: Expr[Double]): Expr[Long] = '((~x).toLong)
    def toFloat(x: Expr[Double]): Expr[Float] = '((~x).toFloat)
    def toDouble(x: Expr[Double]): Expr[Double] = x
    // logic in Numeric base trait mishandles abs(-0.0)
    // override def abs(x: Double): Double = math.abs(x)
  }
  trait DoubleExprIsFractional extends DoubleExprIsConflicted with Fractional[Double] {
    def div(x: Double, y: Double): Double = x / y
  }
  trait DoubleExprAsIfIntegral extends DoubleExprIsConflicted with Integral[Double] {
    def quot(x: Double, y: Double): Double = (BigDecimal(x) quot BigDecimal(y)).doubleValue
    def rem(x: Double, y: Double): Double = (BigDecimal(x) remainder BigDecimal(y)).doubleValue
  }

  trait BigDecimalIsConflicted extends NumericExpr[BigDecimal] {
    def plus(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
    def minus(x: BigDecimal, y: BigDecimal): BigDecimal = x - y
    def times(x: BigDecimal, y: BigDecimal): BigDecimal = x * y
    def negate(x: BigDecimal): BigDecimal = -x
    def fromInt(x: Int): BigDecimal = BigDecimal(x)
    def toInt(x: BigDecimal): Int = x.intValue
    def toLong(x: BigDecimal): Long = x.longValue
    def toFloat(x: BigDecimal): Float = x.floatValue
    def toDouble(x: BigDecimal): Double = x.doubleValue
  }

  trait BigDecimalIsFractional extends BigDecimalIsConflicted with Fractional[BigDecimal] {
    def div(x: BigDecimal, y: BigDecimal): BigDecimal = x / y
  }
  trait BigDecimalAsIfIntegral extends BigDecimalIsConflicted with Integral[BigDecimal] {
    def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x quot y
    def rem(x: BigDecimal, y: BigDecimal): BigDecimal = x remainder y
  }

  // For Double and BigDecimal we offer implicit Fractional objects, but also one
  // which acts like an Integral type, which is useful in NumericRange.
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional with Ordering.BigDecimalOrdering
  object BigDecimalAsIfIntegral extends BigDecimalAsIfIntegral with Ordering.BigDecimalOrdering

  implicit object DoubleExprIsFractional extends DoubleExprIsFractional with Ordering.DoubleOrdering
  object DoubleExprAsIfIntegral extends DoubleExprAsIfIntegral with Ordering.DoubleOrdering
}

trait NumericExpr[T] {
  def plus(x: Expr[T], y: Expr[T]): Expr[T]
  def minus(x: Expr[T], y: Expr[T]): Expr[T]
  def times(x: Expr[T], y: Expr[T]): Expr[T]
  def negate(x: Expr[T]): Expr[T]
  def fromInt(x: Int): Expr[T]
  def toInt(x: Expr[T]): Expr[Int]
  def toLong(x: Expr[T]): Expr[Long]
  def toFloat(x: Expr[T]): Expr[Float]
  def toDouble(x: Expr[T]): Expr[Double]

  def zero: Expr[T] = fromInt(0)
  def one: Expr[T] = fromInt(1)

  // def abs(x: Expr[T]): Expr[T] = if (lt(x, zero)) negate(x) else x
  // def signum(x: Expr[T]): Int =
  //   if (lt(x, zero)) -1
  //   else if (gt(x, zero)) 1
  //   else 0

  class Ops(lhs: Expr[T]) {
    def +(rhs: Expr[T]): Expr[T] = plus(lhs, rhs)
    def -(rhs: Expr[T]): Expr[T] = minus(lhs, rhs)
    def *(rhs: Expr[T]): Expr[T] = times(lhs, rhs)
    def unary_-(): Expr[T] = negate(lhs)
    // def abs(): Expr[T] = NumericExpr.this.abs(lhs)
    // def signum(): Expr[Int] = NumericExpr.this.signum(lhs)
    def toInt(): Expr[Int] = NumericExpr.this.toInt(lhs)
    def toLong(): Expr[Long] = NumericExpr.this.toLong(lhs)
    def toFloat(): Expr[Float] = NumericExpr.this.toFloat(lhs)
    def toDouble(): Expr[Double] = NumericExpr.this.toDouble(lhs)
  }
  implicit def mkNumericOps(lhs: Expr[T]): Ops = new Ops(lhs)
}
