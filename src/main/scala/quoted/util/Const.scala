package scala.quoted
package util

object Const {

  /** Matches expressions containing literal constant values and extracts the value.
   *
   *  - Converts expression containing literal values to their values:
   *    - `'{1}` -> `1`, `'{2}` -> `2`, ...
   *    - For all primitive types and `String`
   *
   *  Usage:
   *  ```
   *  case '{ ... ${expr @ Const(value)}: T ...} =>
   *    // expr: Expr[T]
   *    // value: T
   *  ```
   *
   *  To directly unlift an expression `expr: Expr[T]` consider using `expr.unlift`/`expr.unliftOrError` instead.
   */
  def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
    import quotes.reflect._
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) =>
         c match
          case NullConstant() => None
          case UnitConstant() => None
          case ClassOfConstant(_) => None
          case _ => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.asTerm)
  }
}