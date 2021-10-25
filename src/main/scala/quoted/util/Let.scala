package scala.quoted
package util

object Let {

  /** Create a val reference with value `value` and used in `body`.
   *  `body` receives a `Ref[T]` argument which exposes `ref` and `update`.
   *
   *  `val`('{7}) {
   *    (x: Ref[T]) => '{
   *      2 + ${x.ref}
   *    }
   *  }
   *
   *  will create the equivalent of
   *
   *  '{
   *    val x = 7
   *    2 + x
   *  }
   */
  def `val`[T : Type, U: Type](value: Expr[T])(body: Ref[T] => Expr[U])(using Quotes): Expr[U] = '{
    val x = $value
    ${ body(new Ref('x)) }
  }

  /** Create a val reference with lifted value `value` and used in `body`.
   *  `body` receives a `Static[T]` argument which exposes `ref` and `value`.
   *
   *  static('{7}) {
   *    (x: Static[T]) => '{
   *      ${x.ref} + ${Expr(x.value)}
   *    }
   *  }
   *
   *  will create the equivalent of
   *
   *  '{
   *    val x = 7
   *    x + 7
   *  }
   */
  def static[T : ToExpr : Type, U: Type](value: T)(body: Static[T] => Expr[U])(using Quotes): Expr[U] = '{
    val x = ${Expr(value)}
    ${ body(new Static('x, value)) }
  }

  /** Create a variable initialized with `init` and used in `body`.
   *  `body` receives a `Var[T]` argument which exposes `ref` and `update`.
   *
   *  `var`('{7}) {
   *    x => '{
   *      while(0 < $x)
   *        ${x.update('{$x - 1})}
   *      ${x.get}
   *    }
   *  }
   *
   *  will create the equivalent of
   *
   *  '{
   *    var x = 7
   *    while (0 < x)
   *      x = x - 1
   *     x
   *  }
   */
  def `var`[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U])(using Quotes): Expr[U] = '{
    var x = $init
    ${ body(new Var[T]('x, e => '{ x = $e })) }
  }

  final class Static[T](
    ref: Expr[T],
    /** The value of the field */
    val value: T
  ) extends Ref[T](ref)

  final class Var[T](
    ref: Expr[T],
    /** Creates an update to this variable with the given expression */
    val update:  Expr[T] =>  Expr[Unit]
  ) extends Ref[T](ref)

  sealed class Ref[T](
    /** An expression representing a reference to field */
    val ref: Expr[T]
  )
}
