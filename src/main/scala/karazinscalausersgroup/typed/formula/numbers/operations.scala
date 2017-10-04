package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula._
import karazinscalausersgroup.typed.formula.numbers.Number._

/**
  * @author Igor Wolkov
  */
object operations {

  implicit class `+ Ops`[L <: Expression[Number]](thisValue: L) {
    def +[R <: Expression[Number]](thatValue: R): +[L, R] =
      new +(thisValue, thatValue)
  }

  implicit class `- Ops`[L <: Expression[Number]](thisValue: L) {
    def -[R <: Expression[Number]](thatValue: R): -[L, R] =
      new -(thisValue, thatValue)
  }

  implicit class `unary - Ops`[L <: Expression[Number]](thisValue: L) {
    def unary_- : ~[L] =
      new ~(thisValue)
  }

  implicit class `* Ops`[L <: Expression[Number]](thisValue: L) {
    def *[R <: Expression[Number]](thatValue: R): *[L, R] =
      new *(thisValue, thatValue)
  }

  implicit class `/ Ops`[L <: Expression[Number]](thisValue: L) {
    def /[R <: Expression[Number]](thatValue: R): /[L, R] =
      new /(thisValue, thatValue)
  }

  class +[+L <: Expression[Number], +R <: Expression[Number]](val left: L, val right: R)
    extends Binary[Number, L, R] {
    val v: Number = left.v + right.v

    override def toString = s"(${left.toString} + ${right.toString})"
  }

  class -[+L <: Expression[Number], +R <: Expression[Number]](val left: L, val right: R)
    extends Binary[Number, L, R] {
    val v: Number = left.v - right.v

    override def toString = s"(${left.toString} - ${right.toString})"
  }

  // Unary minus
  class ~[+E <: Expression[Number]](val expression: E)
    extends Unary[Number, E] {
    val v: Number = Number(0) - expression.v

    override def toString = s"-${expression.toString})"
  }

  class *[+L <: Expression[Number], +R <: Expression[Number]](val left: L, val right: R)
    extends Binary[Number, L, R] {
    val v: Number = left.v * right.v

    override def toString = s"(${left.toString} * ${right.toString})"
  }

  class /[+L <: Expression[Number], +R <: Expression[Number]](val left: L, val right: R)
    extends Binary[Number, L, R] {
    val v: Number = left.v / right.v

    override def toString = s"(${left.toString} / ${right.toString})"
  }

  object Σ {
    def apply[E <: Expression[Number]](implicit
                                       foldable2: Foldable[Number, Foldable[Number, Value[Number]]],
                                       defaults: Seq[Value[Number]],
                                       builder: ExpressionBuilder[Number, E]): Σ[E] =
      new Σ(foldable2, defaults, builder)
  }

  class Σ[E <: Expression[Number]](foldable2: Foldable[Number, Foldable[Number, Value[Number]]],
                                   defaults: Seq[Value[Number]],
                                   builder: ExpressionBuilder[Number, E]) extends Expression[Number] {
    val v: Number =
      (foldable2.expressions flatMap { foldable1 =>
        builder.build(foldable1.expressions ++ defaults)
      }: List[E]).foldLeft(Zero) {
        (acc, value) => acc + value.v
      }
  }

}
