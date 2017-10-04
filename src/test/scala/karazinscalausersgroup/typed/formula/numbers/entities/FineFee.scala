package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.numbers._

/**
  * @author Igor Wolkov
  */
trait FineFee

// TODO: Add macros to generate this boilerplate
case class PreviousFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = PreviousFineFee

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"PreviousFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class CurrentFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

   override type Self = CurrentFineFee

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"CurrentFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}

// TODO: Add macros to generate this boilerplate
case class NextFineFee(amount: Number, expressions: List[Covering] = Nil)
  extends InstallmentEntity {

  override type Self = NextFineFee

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[Covering]): Self = copy(amount, expressions)

  // TODO: Rewrite, add additional information
  override def toString = s"NextFineFee(totalAmount: $v, amount: $amount, coverings: ${ expressions mkString ", " })"
}


