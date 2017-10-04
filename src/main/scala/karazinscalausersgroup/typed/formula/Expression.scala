package karazinscalausersgroup.typed.formula

import scala.annotation.unchecked.uncheckedVariance

/**
  * @author Igor Wolkov
  */

trait Expression[+T] {
  def v: T

  override def toString = s"Expression(${v.toString})"
}

trait Value[+T] extends Expression[T]

trait Unary[+T, +E <: Expression[T]] extends Expression[T] {
  val expression: E
}

trait Binary[+T, +A <: Expression[T], +B <: Expression[T]] extends Expression[T] {
  val left: A
  val right: B
}

trait Foldable[T, +E <: Expression[T]] extends Expression[T] {
  def expressions: List[E]
  val op: (Expression[T], E @uncheckedVariance) => Expression[T]
  val initial: Expression[T]

  lazy val v: T =
    expressions.foldLeft(initial) {
      case (acc, expression) => op(acc, expression)
    }.v
}

