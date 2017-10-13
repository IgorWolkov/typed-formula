package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.generic.entity
import karazinscalausersgroup.typed.formula.numbers._

/**
  * @author Igor Wolkov
  */
trait FineFee

@entity
case class PreviousFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class CurrentFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class NextFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"NextFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}


