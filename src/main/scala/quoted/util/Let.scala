package scala.quoted
package util

import scala.quoted.util.Unrolled._

object Let {

  /** Create a val reference with value `value` and used in `body`.
   *  `body` recieves a `Ref[T]` argument which exposes `ref` and `update`.
   *
   *  `val`('(7)) {
   *    (x: Ref[T]) => '{
   *      2 + ~x.ref
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
  def `val`[T : Type, U](value: Expr[T])(body: Ref[T] => Expr[U]): Expr[U] = '{
    val x = ~value
    ~body(new Ref('(x)))
  }

  /** Create a val reference with lifted value `value` and used in `body`.
   *  `body` recieves a `Static[T]` argument which exposes `ref` and `value`.
   *
   *  static('(7)) {
   *    (x: Static[T]) => '{
   *      ~x.ref + ~x.value.toExpr
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
  def static[T : Liftable : Type, U](x: T)(body: Static[T] => Expr[U]): Expr[U] = '{
    val static_x = ~x.toExpr
    ~body(new Static('(static_x), x))
  }

  /** Create a varaiable initialized with `init` and used in `body`.
   *  `body` recieves a `Var[T]` argument which exposes `ref` and `update`.
   *
   *  `var`('(7)) {
   *    x => '{
   *      while(0 < ~x)
   *        ~x.update('(~x - 1))
   *      ~x.get
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
  def `var`[T: Type, U](init: Expr[T])(body: Var[T] => Expr[U]): Expr[U] = '{
    var x = ~init
    ~body(new Var[T]('(x), e => '{ x = ~e }))
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
