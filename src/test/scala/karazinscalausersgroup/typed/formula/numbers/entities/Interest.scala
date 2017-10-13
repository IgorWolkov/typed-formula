package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.generic.entity
import karazinscalausersgroup.typed.formula.numbers.Number

/**
  * @author Igor Wolkov
  */
trait Interest

@entity
case class PreviousInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class CurrentInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class NextInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"NextInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}


