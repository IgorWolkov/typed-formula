package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.generic.entity
import karazinscalausersgroup.typed.formula.numbers.Number

/**
  * @author Igor Wolkov
  */
trait Principal

@entity
case class PreviousPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class CurrentPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

@entity
case class NextPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  // TODO: Rewrite, add additional information
  override def toString = s"NextPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}
