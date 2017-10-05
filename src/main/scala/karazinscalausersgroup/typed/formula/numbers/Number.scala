package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula.{Foldable, Expression, Value}

import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions

/**
  * @author Igor Wolkov
  */
trait Number {
  type N
  def v: N

  def +(that: Number): Number
  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number
  def unary_- : Number

  def <(that: Number): Boolean
  def ==(that: Number): Boolean
  def !=(that: Number): Boolean
  def >(that: Number): Boolean

  def <=(that: Number): Boolean =
    this < that || this == that

  def >=(that: Number): Boolean =
    this > that || this == that

}

object Number {

  implicit def apply(int: Int): IntNumber = IntNumber(int)

  implicit def apply(long: Long): LongNumber = LongNumber(long)

  implicit def apply(double: Double): DoubleNumber = DoubleNumber(double)

  val Zero: Number = IntNumber(0)
  val One: Number = IntNumber(1)

  def min(left: Number, right: Number): Number =
    if(left < right) left
    else             right

  def max(left: Number, right: Number): Number =
    if(left > right) left
    else             right

  case class IntNumber(v: Int) extends Number {
    type N = Int

    def +(that: Number): Number =
      that match {
        case IntNumber(r)    => IntNumber(v + r)
        case LongNumber(r)   => LongNumber(v + r)
        case DoubleNumber(r) => DoubleNumber(v + r)
      }

    def unary_- : Number = Zero - this

    def -(that: Number): Number =
      that match {
        case IntNumber(r)    => IntNumber(v - r)
        case LongNumber(r)   => LongNumber(v - r)
        case DoubleNumber(r) => DoubleNumber(v - r)
      }

    def *(that: Number): Number =
      that match {
        case IntNumber(r)    => IntNumber(v * r)
        case LongNumber(r)   => LongNumber(v * r)
        case DoubleNumber(r) => DoubleNumber(v * r)
      }

    def /(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v.toDouble / r)
        case LongNumber(r)   => DoubleNumber(v.toDouble / r)
        case DoubleNumber(r) => DoubleNumber(v.toDouble / r)
      }

    def <(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v < r
        case LongNumber(r)   => v < r
        case DoubleNumber(r) => v < r
      }

    def ==(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v == r
        case LongNumber(r)   => v == r
        case DoubleNumber(r) => v == r
      }

    def !=(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v != r
        case LongNumber(r)   => v != r
        case DoubleNumber(r) => v != r
      }

    def >(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v > r
        case LongNumber(r)   => v > r
        case DoubleNumber(r) => v > r
      }
  }

  case class LongNumber(v: Long) extends Number {
    type N = Long

    def +(that: Number): Number =
      that match {
        case IntNumber(r)    => LongNumber(v + r)
        case LongNumber(r)   => LongNumber(v + r)
        case DoubleNumber(r) => DoubleNumber(v + r)
      }

    def unary_- : Number = Zero - this

    def -(that: Number): Number =
      that match {
        case IntNumber(r)    => LongNumber(v - r)
        case LongNumber(r)   => LongNumber(v - r)
        case DoubleNumber(r) => DoubleNumber(v - r)
      }

    def *(that: Number): Number =
      that match {
        case IntNumber(r)    => LongNumber(v * r)
        case LongNumber(r)   => LongNumber(v * r)
        case DoubleNumber(r) => DoubleNumber(v * r)
      }

    def /(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v.toDouble / r)
        case LongNumber(r)   => DoubleNumber(v.toDouble / r)
        case DoubleNumber(r) => DoubleNumber(v.toDouble / r)
      }

    def <(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v < r
        case LongNumber(r)   => v < r
        case DoubleNumber(r) => v < r
      }

    def ==(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v == r
        case LongNumber(r)   => v == r
        case DoubleNumber(r) => v == r
      }

    def !=(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v != r
        case LongNumber(r)   => v != r
        case DoubleNumber(r) => v != r
      }

    def >(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v > r
        case LongNumber(r)   => v > r
        case DoubleNumber(r) => v > r
      }
  }

  case class DoubleNumber(v: Double) extends Number {
    type N = Double

    def +(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v + r)
        case LongNumber(r)   => DoubleNumber(v + r)
        case DoubleNumber(r) => DoubleNumber(v + r)
      }

    def unary_- : Number = Zero - this

    def -(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v - r)
        case LongNumber(r)   => DoubleNumber(v - r)
        case DoubleNumber(r) => DoubleNumber(v - r)
      }

    def *(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v * r)
        case LongNumber(r)   => DoubleNumber(v * r)
        case DoubleNumber(r) => DoubleNumber(v * r)
      }

    def /(that: Number): Number =
      that match {
        case IntNumber(r)    => DoubleNumber(v / r)
        case LongNumber(r)   => DoubleNumber(v / r)
        case DoubleNumber(r) => DoubleNumber(v / r)
      }

    def <(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v < r
        case LongNumber(r)   => v < r
        case DoubleNumber(r) => v < r
      }

    def ==(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v == r
        case LongNumber(r)   => v == r
        case DoubleNumber(r) => v == r
      }

    def !=(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v != r
        case LongNumber(r)   => v != r
        case DoubleNumber(r) => v != r
      }

    def >(that: Number): Boolean =
      that match {
        case IntNumber(r)    => v > r
        case LongNumber(r)   => v > r
        case DoubleNumber(r) => v > r
      }
  }
}

abstract class AbstractFoldable[+E <: Expression[Number]](val expressions: List[E],
                                                          val op: (Expression[Number], E @uncheckedVariance) => Expression[Number])
  extends Foldable[Number, E] {
  val initial: Value[Number] = Value(Number(0))
}

object Value {
  def apply[T <: Number](number: T): Value[Number] =
    new Value[Number] {
      val v: Number = number
    }
}
