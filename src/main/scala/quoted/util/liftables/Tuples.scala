package scala.quoted
package util.liftables

object Tuples {
  implicit def Tuple1IsLiftable[T1 : Liftable : Type]: Liftable[Tuple1[T1]] = {
     case Tuple1(x1: T1) => '{ Tuple1(~(x1: Expr[T1])) }
  }

  implicit def Tuple2IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type]: Liftable[(T1, T2)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2])) }
  }

  implicit def Tuple3IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type]: Liftable[(T1, T2, T3)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3])) }
  }

  implicit def Tuple4IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type]: Liftable[(T1, T2, T3, T4)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4])) }
  }

  implicit def Tuple5IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5])) }
  }

  implicit def Tuple6IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6])) }
  }

  implicit def Tuple7IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7])) }
  }

  implicit def Tuple8IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8])) }
  }

  implicit def Tuple9IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7:Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9])) }
  }

  implicit def Tuple10IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10])) }
  }

  implicit def Tuple11IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11])) }
  }

  implicit def Tuple12IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6:Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12])) }
  }

  implicit def Tuple13IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13])) }
  }

  implicit def Tuple14IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14])) }
  }

  implicit def Tuple15IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15])) }
  }

  implicit def Tuple16IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8,T9, T10, T11, T12, T13, T14, T15, T16)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16])) }
  }

  implicit def Tuple17IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17])) }
  }

  implicit def Tuple18IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18])) }
  }

  implicit def Tuple19IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19])) }
  }

  implicit def Tuple20IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]= {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20])) }
  }

  implicit def Tuple21IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type, T21 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14,T15, T16, T17, T18, T19, T20, T21)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20]), ~(x._21: Expr[T21])) }
  }

  implicit def Tuple22IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type, T21 : Liftable : Type, T22 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = {
    x => '{ (~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20]), ~(x._21:Expr[T21]), ~(x._22: Expr[T22])) }
  }

}
