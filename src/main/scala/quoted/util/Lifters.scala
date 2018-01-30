package scala.quoted
package util

import scala.quoted.Liftable._

object Lifters {

  // TODO add it to scala.quoted.Liftable
  implicit def UnitIsLiftable: Liftable[Unit] = {
    _  => '{ () }
  }

  implicit def OptionIsLiftable[T : Liftable : Type]: Liftable[Option[T]] = {
    case Some(x)  => '{ Some(~x.toExpr): Option[T] }
    case None => '{ None: Option[T] }
  }

  // Seq lifters

  implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ List.empty[T] }
  }

  // Tuple lifters

  implicit def Tuple1IsLiftable[T1 : Liftable : Type]: Liftable[Tuple1[T1]] = {
    x => '{ Tuple1(~x._1.toExpr) }
  }

  implicit def Tuple2IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type]: Liftable[(T1, T2)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr) }
  }

  implicit def Tuple3IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type]: Liftable[(T1, T2, T3)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr) }
  }

  implicit def Tuple4IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type]: Liftable[(T1, T2, T3, T4)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr) }
  }

  implicit def Tuple5IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr) }
  }

  implicit def Tuple6IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr) }
  }

  implicit def Tuple7IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr) }
  }

  implicit def Tuple8IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr) }
  }

  implicit def Tuple9IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr) }
  }

  implicit def Tuple10IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr) }
  }

  implicit def Tuple11IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr) }
  }

  implicit def Tuple12IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr) }
  }

  implicit def Tuple13IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr) }
  }

  implicit def Tuple14IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr) }
  }

  implicit def Tuple15IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr) }
  }

  implicit def Tuple16IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8,T9, T10, T11, T12, T13, T14, T15, T16)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr) }
  }

  implicit def Tuple17IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr) }
  }

  implicit def Tuple18IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr, ~x._18.toExpr) }
  }

  implicit def Tuple19IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr, ~x._18.toExpr, ~x._19.toExpr) }
  }

  implicit def Tuple20IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]= {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr, ~x._18.toExpr, ~x._19.toExpr, ~x._20.toExpr) }
  }

  implicit def Tuple21IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type, T21 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14,T15, T16, T17, T18, T19, T20, T21)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr, ~x._18.toExpr, ~x._19.toExpr, ~x._20.toExpr, ~x._21.toExpr) }
  }

  implicit def Tuple22IsLiftable[T1 : Liftable : Type, T2 : Liftable : Type, T3 : Liftable : Type, T4 : Liftable : Type, T5 : Liftable : Type, T6 : Liftable : Type, T7 : Liftable : Type, T8 : Liftable : Type, T9 : Liftable : Type, T10 : Liftable : Type, T11 : Liftable : Type, T12 : Liftable : Type, T13 : Liftable : Type, T14 : Liftable : Type, T15 : Liftable : Type, T16 : Liftable : Type, T17 : Liftable : Type, T18 : Liftable : Type, T19 : Liftable : Type, T20 : Liftable : Type, T21 : Liftable : Type, T22 : Liftable : Type]: Liftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = {
    x => '{ (~x._1.toExpr, ~x._2.toExpr, ~x._3.toExpr, ~x._4.toExpr, ~x._5.toExpr, ~x._6.toExpr, ~x._7.toExpr, ~x._8.toExpr, ~x._9.toExpr, ~x._10.toExpr, ~x._11.toExpr, ~x._12.toExpr, ~x._13.toExpr, ~x._14.toExpr, ~x._15.toExpr, ~x._16.toExpr, ~x._17.toExpr, ~x._18.toExpr, ~x._19.toExpr, ~x._20.toExpr, ~x._21.toExpr, ~x._22.toExpr) }
  }

}