package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.generic.entity

/**
  * @author Igor Wolkov
  */
@entity
case class Loan(expressions: List[Installment]) extends FoldableLoanEntity[Installment] {

  lazy val amount = v

  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  def addInstallment(installment: Installment): Self = copy(expressions ::: installment :: Nil)

  def addInstallments(installments: List[Installment]): Self = copy(expressions ::: installments)

  override def toString = s"Loan(${expressions map { _.toString }  mkString ", "})"

}
