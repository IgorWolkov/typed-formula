package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.numbers.Number

/**
  * @author Igor Wolkov
  */
trait Interest

// TODO: Add macros to generate this boilerplate
case class PreviousInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = PreviousInterest

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class CurrentInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = CurrentInterest

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class NextInterest(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = NextInterest

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"NextInterest(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}


