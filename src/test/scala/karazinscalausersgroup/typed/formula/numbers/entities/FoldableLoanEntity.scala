package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula._
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.numbers.{Number, Value}

import scala.annotation.unchecked.uncheckedVariance

/**
  * @author Igor Wolkov
  */

trait LoanEvent extends Event[Number] {
  def description: String
}

object LoanEvent {
  def apply(value: Number, description: String): NonEmptyLoanEvent = NonEmptyLoanEvent(value, description)
}

trait LoanLimit extends Limit[Number]

case class NonEmptyLoanEvent(value: Number, description: String) extends LoanEvent {
  type R = NonEmptyLoanEvent

  def withLimit(number: Number): NonEmptyLoanEvent with Limit[Number] =
    new NonEmptyLoanEvent(value: Number, description) with Limit[Number] {
      def limit: Number = number
    }
}

case class EmptyLoanEvent(description: String) extends LoanEvent {
  type R = EmptyLoanEvent

  def withLimit(number: Number): EmptyLoanEvent with Limit[Number] =
    new EmptyLoanEvent(description) with Limit[Number] {
      def limit = number
    }

  override def toString = "EmptyLoanEvent"
}

trait LoanEntity extends Entity[Number, LoanEvent]

trait ValueLoanEntity extends LoanEntity with ValueEntity[Number, LoanEvent]

case class Covering(amount: Number, description: String) extends ValueLoanEntity {
  type Self = Covering

  def v: Number = amount
  def applyEvent(event: LoanEvent) = (copy(), event)
}

trait FoldableLoanEntity[+Entity <: LoanEntity]
  extends LoanEntity with FoldableEntity[Number, LoanEvent, Entity] {

  type Self <: FoldableLoanEntity[Entity]

  val amount: Number

  val expressions: List[Entity]

  lazy val initial: Expression[Number] = Value(amount)

  lazy val op: (Expression[Number], Entity @uncheckedVariance) => Expression[Number] =
    (acc, value) => acc + value
}


