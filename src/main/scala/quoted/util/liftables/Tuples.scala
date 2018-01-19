package scala.quoted
package util.liftables


object Tuples {
  implicit def Tuple1IsLiftable[T1: Liftable](implicit t1: Type[T1]): Liftable[Tuple1[T1]] = {
     case Tuple1(x1: T1) => '{ Tuple1[~t1](~(x1: Expr[T1])) }
   }

   implicit def Tuple2IsLiftable[T1: Liftable, T2: Liftable](implicit t1: Type[T1], t2: Type[T2]): Liftable[(T1, T2)] = {
     x => '{ Tuple2[~t1, ~t2](~(x._1: Expr[T1]), ~(x._2: Expr[T2])) }
   }

   implicit def Tuple3IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3]): Liftable[(T1, T2, T3)] = {
     x => '{ Tuple3[~t1, ~t2, ~t3](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3])) }
   }

   implicit def Tuple4IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4]): Liftable[(T1, T2, T3, T4)] = {
     x => '{ Tuple4[~t1, ~t2, ~t3, ~t4](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4])) }
   }

   // TODO more tuples

}
