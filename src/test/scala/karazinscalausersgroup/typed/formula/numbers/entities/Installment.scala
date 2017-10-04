package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.Limit
import karazinscalausersgroup.typed.formula.numbers.Number
import karazinscalausersgroup.typed.formula.numbers.Number.Zero

/**
  * @author Igor Wolkov
  */

trait InstallmentEntity extends ValueLoanEntity with FoldableLoanEntity[Covering] {
  type Self <: InstallmentEntity

  def copyEntities(expressions: List[Covering]): Self

  def applyEvent(event: LoanEvent): (Self, LoanEvent) =
    event match {
      case e: (NonEmptyLoanEvent with Limit[Number]) =>
        val eventAmount = e.value
        val eventLimit = e.limit
        val entityAmount = this.v

        // TODO: Simplify and describe this logic
        if(entityAmount == Zero) {
          (copyEntity(), event)
        } else if (eventAmount <= eventLimit) {
          if (entityAmount + eventAmount < Number(0) || entityAmount + eventAmount == Number(0)) {
            (copyEntities( expressions ::: Covering(eventAmount) :: Nil), EmptyLoanEvent(event.description))
          } else {
            (copyEntities(expressions ::: Covering(Number(0) - entityAmount) :: Nil),
              NonEmptyLoanEvent(eventAmount + entityAmount, event.description))
          }
        }
        else {
          if (entityAmount + eventLimit < Number(0) || entityAmount + eventLimit == Number(0)) {
            (copyEntities(expressions ::: Covering(eventLimit) :: Nil), NonEmptyLoanEvent(eventAmount - eventLimit, event.description))
          } else {
            (copyEntities(expressions ::: Covering(Number(0) - entityAmount) :: Nil), NonEmptyLoanEvent(eventAmount + entityAmount, event.description))
          }
        }

      case empty: EmptyLoanEvent =>
        (copyEntity(), empty)
    }
}

abstract class Installment(val entities: List[InstallmentEntity]) extends FoldableLoanEntity[InstallmentEntity] {
  type Self <: Installment
}

// TODO: Add macros to generate this boilerplate
case class PreviousInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  type Self = PreviousInstallment

  val amount = Zero

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def applyEvent(event: LoanEvent) = ???

  override def toString = s"PreviousInstallment(${expressions map { _.toString }  mkString ", "})"

}

// TODO: Add macros to generate this boilerplate
case class CurrentInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  override type Self = CurrentInstallment

  val amount = Zero

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def applyEvent(event: LoanEvent) = ???

  override def toString = s"CurrentInstallment(${expressions map { _.toString }  mkString ", "})"

}

// TODO: Add macros to generate this boilerplate
case class NextInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  override type Self = NextInstallment

  val amount = Zero

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def applyEvent(event: LoanEvent) = ???

  override def toString = s"NextInstallment(${expressions map { _.toString }  mkString ", "})"

}
