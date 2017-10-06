package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.numbers._

/**
  * @author Igor Wolkov
  */
// TODO: Add macros to generate this boilerplate
case class EarlyRepaymentFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = EarlyRepaymentFee

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"EarlyRepaymentFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}
