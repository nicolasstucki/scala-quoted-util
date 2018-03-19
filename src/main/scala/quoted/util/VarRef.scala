package scala.quoted
package util

object VarRef {
  /** Create a varaiable initialized with `init` and used in `body`.
   *  `body` recieves two arguments, the first is a referece to the varable and
   *  the second is a function that an asignment to the variable.
   *
   *  VarRef('(7)) {
   *    (x, update_x) => '{
   *      while(0 < ~x)
   *        ~update_x('(~x - 1))
   *      ~x
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
  def apply[T: Type, U](init: Expr[T])(body: (Expr[T], Expr[T] => Expr[Unit]) => Expr[U]): Expr[U] = '{
    var x = ~init
    ~body('(x), e => '{ x = ~e })
  }

}
