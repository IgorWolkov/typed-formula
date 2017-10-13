package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.generic.entity
import karazinscalausersgroup.typed.formula.numbers._

/**
  * @author Igor Wolkov
  */
@entity
case class EarlyRepaymentFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override def toString = s"EarlyRepaymentFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"

}
