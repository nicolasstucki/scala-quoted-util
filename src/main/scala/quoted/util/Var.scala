package scala.quoted
package util

sealed trait Var[T] {
  def get: Expr[T]
  def update(x: Expr[T]): Expr[Unit]
}

object Var {
  /** Create a varaiable initialized with `init` and used in `body`.
   *  `body` recieves two arguments, the first is a referece to the varable and
   *  the second is a function that an asignment to the variable.
   *
   *  Var('(7)) {
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
  def apply[T: Type, U](init: Expr[T])(body: Var[T] => Expr[U]): Expr[U] = '{
    var x = ~init
    ~body(
      new Var[T] {
        def get: Expr[T] = '(x)
        def update(e: Expr[T]): Expr[Unit] = '{ x = ~e }
      }
    )
  }

}
