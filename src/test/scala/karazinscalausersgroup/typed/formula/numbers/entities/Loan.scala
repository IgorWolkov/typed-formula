package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.numbers.Number.Zero

/**
  * @author Igor Wolkov
  */
// TODO: Add macros to generate this boilerplate
case class Loan(expressions: List[Installment]) extends FoldableLoanEntity[Installment] {

  override type Self = Loan

  val amount = Zero

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Installment]): Self = copy(expressions)

  def applyEvent(event: LoanEvent) = ???

  override def toString = s"Loan(${expressions map { _.toString }  mkString ", "})"

}
