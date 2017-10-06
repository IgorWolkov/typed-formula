package karazinscalausersgroup.typed.formula.numbers.entities

/**
  * @author Igor Wolkov
  */
// TODO: Add macros to generate this boilerplate
case class Loan(expressions: List[Installment]) extends FoldableLoanEntity[Installment] {

  override type Self = Loan

  lazy val amount = v

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Installment]): Self = copy(expressions)

  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  override def toString = s"Loan(${expressions map { _.toString }  mkString ", "})"

}
