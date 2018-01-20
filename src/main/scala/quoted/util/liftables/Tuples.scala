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

  implicit def Tuple5IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5]): Liftable[(T1, T2, T3, T4, T5)] = {
    x => '{ Tuple5[~t1, ~t2, ~t3, ~t4, ~t5](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5])) }
  }

  implicit def Tuple6IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6]): Liftable[(T1, T2, T3, T4, T5, T6)] = {
    x => '{ Tuple6[~t1, ~t2, ~t3, ~t4, ~t5, ~t6](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6])) }
  }

  implicit def Tuple7IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7]): Liftable[(T1, T2, T3, T4, T5, T6, T7)] = {
    x => '{ Tuple7[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7])) }
  }

  implicit def Tuple8IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
    x => '{ Tuple8[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8])) }
  }

  implicit def Tuple9IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    x => '{ Tuple9[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7:Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9])) }
  }

  implicit def Tuple10IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    x => '{ Tuple10[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10])) }
  }

  implicit def Tuple11IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    x => '{ Tuple11[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11])) }
  }

  implicit def Tuple12IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    x => '{ Tuple12[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6:Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12])) }
  }

  implicit def Tuple13IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10],t11: Type[T11], t12: Type[T12], t13: Type[T13]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    x => '{ Tuple13[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13])) }
  }

  implicit def Tuple14IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    x => '{ Tuple14[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14])) }
  }

  implicit def Tuple15IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8],t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    x => '{ Tuple15[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15])) }
  }

  implicit def Tuple16IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8,T9, T10, T11, T12, T13, T14, T15, T16)] = {
    x => '{ Tuple16[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16])) }
  }

  implicit def Tuple17IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16], t17: Type[T17]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = {
    x => '{ Tuple17[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17])) }
  }

  implicit def Tuple18IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16], t17: Type[T17], t18: Type[T18]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
    x => '{ Tuple18[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17, ~t18](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18])) }
  }

  implicit def Tuple19IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16], t17: Type[T17], t18: Type[T18], t19: Type[T19]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = {
    x => '{ Tuple19[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17, ~t18, ~t19](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19])) }
  }

  implicit def Tuple20IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16], t17: Type[T17], t18: Type[T18], t19: Type[T19], t20: Type[T20]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]= {
    x => '{ Tuple20[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17, ~t18, ~t19, ~t20](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20])) }
  }

  implicit def Tuple21IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable, T21: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14],t15: Type[T15], t16: Type[T16], t17: Type[T17], t18: Type[T18], t19: Type[T19], t20: Type[T20], t21: Type[T21]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14,T15, T16, T17, T18, T19, T20, T21)] = {
    x => '{ Tuple21[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17, ~t18, ~t19, ~t20, ~t21](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20]), ~(x._21: Expr[T21])) }
  }

  implicit def Tuple22IsLiftable[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable, T21: Liftable, T22: Liftable](implicit t1: Type[T1], t2: Type[T2], t3: Type[T3], t4: Type[T4], t5: Type[T5], t6: Type[T6], t7: Type[T7], t8: Type[T8], t9: Type[T9], t10: Type[T10], t11: Type[T11], t12: Type[T12], t13: Type[T13], t14: Type[T14], t15: Type[T15], t16: Type[T16], t17: Type[T17], t18: Type[T18], t19: Type[T19], t20: Type[T20], t21: Type[T21], t22: Type[T22]): Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = {
    x => '{ Tuple22[~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8, ~t9, ~t10, ~t11, ~t12, ~t13, ~t14, ~t15, ~t16, ~t17, ~t18, ~t19, ~t20, ~t21, ~t22](~(x._1: Expr[T1]), ~(x._2: Expr[T2]), ~(x._3: Expr[T3]), ~(x._4: Expr[T4]), ~(x._5: Expr[T5]), ~(x._6: Expr[T6]), ~(x._7: Expr[T7]), ~(x._8: Expr[T8]), ~(x._9: Expr[T9]), ~(x._10: Expr[T10]), ~(x._11: Expr[T11]), ~(x._12: Expr[T12]), ~(x._13: Expr[T13]), ~(x._14: Expr[T14]), ~(x._15: Expr[T15]), ~(x._16: Expr[T16]), ~(x._17: Expr[T17]), ~(x._18: Expr[T18]), ~(x._19: Expr[T19]), ~(x._20: Expr[T20]), ~(x._21:Expr[T21]), ~(x._22: Expr[T22])) }
  }

}
