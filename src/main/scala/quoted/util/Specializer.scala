package scala.quoted
package util

import scala.reflect.ClassTag

object Specializer {

   def specializedEquals[T: Type](a: Expr[T], b: Expr[T]): Expr[Boolean] = specialize(a :: b :: Nil) {
      case BooleanExprList(a :: b :: Nil) => '{ ~a == ~b }
      case ByteExprList(a :: b :: Nil) => '{ ~a == ~b }
      case CharExprList(a :: b :: Nil) => '{ ~a == ~b }
      case ShortExprList(a :: b :: Nil) => '{ ~a == ~b }
      case IntExprList(a :: b :: Nil) => '{ ~a == ~b }
      case LongExprList(a :: b :: Nil) => '{ ~a == ~b }
      case FloatExprList(a :: b :: Nil) => '{ ~a == ~b }
      case DoubleExprList(a :: b :: Nil) => '{ ~a == ~b }
      case _ => '{ ~a == ~b }
    }

  def specialize[T, U](expr: Expr[T])(body: SpecializedExpr[T] => Expr[U])(implicit t: Type[T]): Expr[U] = t match {
    case t: Types.TaggedType[_] if t.ct == ClassTag.Unit => body(UnitExpr(expr.asInstanceOf[Expr[Unit]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Boolean => body(BooleanExpr(expr.asInstanceOf[Expr[Boolean]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Byte => body(ByteExpr(expr.asInstanceOf[Expr[Byte]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Char => body(CharExpr(expr.asInstanceOf[Expr[Char]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Short => body(ShortExpr(expr.asInstanceOf[Expr[Short]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Int => body(IntExpr(expr.asInstanceOf[Expr[Int]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Long => body(LongExpr(expr.asInstanceOf[Expr[Long]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Float => body(FloatExpr(expr.asInstanceOf[Expr[Float]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Double => body(DoubleExpr(expr.asInstanceOf[Expr[Double]]))
    case _ => body(GenericExpr(expr))
  }

  def specialize[T, U](exprs: List[Expr[T]])(body: SpecializedExprList[T] => Expr[U])(implicit t: Type[T]): Expr[U] = t match {
    case t: Types.TaggedType[_] if t.ct == ClassTag.Unit => body(UnitExprList(exprs.asInstanceOf[List[Expr[Unit]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Boolean => body(BooleanExprList(exprs.asInstanceOf[List[Expr[Boolean]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Byte => body(ByteExprList(exprs.asInstanceOf[List[Expr[Byte]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Char => body(CharExprList(exprs.asInstanceOf[List[Expr[Char]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Short => body(ShortExprList(exprs.asInstanceOf[List[Expr[Short]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Int => body(IntExprList(exprs.asInstanceOf[List[Expr[Int]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Long => body(LongExprList(exprs.asInstanceOf[List[Expr[Long]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Float => body(FloatExprList(exprs.asInstanceOf[List[Expr[Float]]]))
    case t: Types.TaggedType[_] if t.ct == ClassTag.Double => body(DoubleExprList(exprs.asInstanceOf[List[Expr[Double]]]))
    case _ => body(GenericExprList(exprs))
  }

  sealed trait SpecializedExpr[+T]
  final case class GenericExpr[T](expr: Expr[T]) extends SpecializedExpr[T]
  final case class UnitExpr(expr: Expr[Unit]) extends SpecializedExpr[Nothing]
  final case class BooleanExpr(expr: Expr[Boolean]) extends SpecializedExpr[Nothing]
  final case class ByteExpr(expr: Expr[Byte]) extends SpecializedExpr[Nothing]
  final case class CharExpr(expr: Expr[Char]) extends SpecializedExpr[Nothing]
  final case class ShortExpr(expr: Expr[Short]) extends SpecializedExpr[Nothing]
  final case class IntExpr(expr: Expr[Int]) extends SpecializedExpr[Nothing]
  final case class LongExpr(expr: Expr[Long]) extends SpecializedExpr[Nothing]
  final case class FloatExpr(expr: Expr[Float]) extends SpecializedExpr[Nothing]
  final case class DoubleExpr(expr: Expr[Double]) extends SpecializedExpr[Nothing]

  sealed trait SpecializedExprList[+T]
  final case class GenericExprList[T](exprs: List[Expr[T]]) extends SpecializedExprList[T]
  final case class UnitExprList(exprs: List[Expr[Unit]]) extends SpecializedExprList[Nothing]
  final case class BooleanExprList(exprs: List[Expr[Boolean]]) extends SpecializedExprList[Nothing]
  final case class ByteExprList(exprs: List[Expr[Byte]]) extends SpecializedExprList[Nothing]
  final case class CharExprList(exprs: List[Expr[Char]]) extends SpecializedExprList[Nothing]
  final case class ShortExprList(exprs: List[Expr[Short]]) extends SpecializedExprList[Nothing]
  final case class IntExprList(exprs: List[Expr[Int]]) extends SpecializedExprList[Nothing]
  final case class LongExprList(exprs: List[Expr[Long]]) extends SpecializedExprList[Nothing]
  final case class FloatExprList(exprs: List[Expr[Float]]) extends SpecializedExprList[Nothing]
  final case class DoubleExprList(exprs: List[Expr[Double]]) extends SpecializedExprList[Nothing]
}
