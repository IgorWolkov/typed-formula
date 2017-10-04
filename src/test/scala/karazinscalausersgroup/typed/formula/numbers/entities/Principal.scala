package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.numbers.Number

/**
  * @author Igor Wolkov
  */
trait Principal

// TODO: Add macros to generate this boilerplate
case class PreviousPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = PreviousPrincipal

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class CurrentPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = CurrentPrincipal

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class NextPrincipal(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = NextPrincipal

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"NextPrincipal(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}
