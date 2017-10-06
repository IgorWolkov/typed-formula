package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.Limit
import karazinscalausersgroup.typed.formula.numbers.Number
import karazinscalausersgroup.typed.formula.numbers.Number.Zero

/**
  * @author Igor Wolkov
  */

trait InstallmentEntity extends ValueLoanEntity with FoldableLoanEntity[Covering] {
  type Self <: InstallmentEntity

  def copyEntities(expressions: List[Covering]): Self

  def applyEvent(event: LoanEvent): (Self, LoanEvent) =
    event match {
      case e: (NonEmptyLoanEvent with Limit[Number]) =>
        val eventAmount = e.value
        val eventLimit = e.limit
        val entityAmount = this.v

        // TODO: Simplify and describe this logic
        if(entityAmount == Zero) {
          (copyEntity(), event)
        } else if (eventAmount <= eventLimit) {
          if (entityAmount + eventAmount < Number(0) || entityAmount + eventAmount == Number(0)) {
            (copyEntities( expressions ::: Covering(eventAmount, e.description) :: Nil), EmptyLoanEvent(event.description))
          } else {
            (copyEntities(expressions ::: Covering(Number(0) - entityAmount, e.description) :: Nil),
              NonEmptyLoanEvent(eventAmount + entityAmount, event.description))
          }
        }
        else {
          if (entityAmount + eventLimit < Zero || entityAmount + eventLimit == Zero) {
            (copyEntities(expressions ::: Covering(eventLimit, e.description) :: Nil), NonEmptyLoanEvent(eventAmount - eventLimit, event.description))
          } else {
            (copyEntities(expressions ::: Covering(-entityAmount, e.description) :: Nil), NonEmptyLoanEvent(eventAmount + entityAmount, event.description))
          }
        }

      case empty: EmptyLoanEvent =>
        (copyEntity(), empty)
    }
}

abstract class Installment(val entities: List[InstallmentEntity]) extends FoldableLoanEntity[InstallmentEntity] {
  type Self <: Installment

  lazy val amount = v
}

// TODO: Add macros to generate this boilerplate
case class PreviousInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  type Self = PreviousInstallment

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def addEntity(entity: InstallmentEntity): Self = copyEntities(expressions ::: entity :: Nil)

  def addEntities(entities: List[InstallmentEntity]): Self = copyEntities(expressions ::: entities)


  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  override def toString = s"PreviousInstallment(${expressions map { _.toString }  mkString ", "})"

}

// TODO: Add macros to generate this boilerplate
case class CurrentInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  override type Self = CurrentInstallment

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def addEntity(entity: InstallmentEntity): Self = copyEntities(expressions ::: entity :: Nil)

  def addEntities(entities: List[InstallmentEntity]): Self = copyEntities(expressions ::: entities)


  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  override def toString = s"CurrentInstallment(${expressions map { _.toString }  mkString ", "})"

}

// TODO: Add macros to generate this boilerplate
case class NextInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  override type Self = NextInstallment

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def addEntity(entity: InstallmentEntity): Self = copyEntities(expressions ::: entity :: Nil)

  def addEntities(entities: List[InstallmentEntity]): Self = copyEntities(expressions ::: entities)


  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  override def toString = s"NextInstallment(${expressions map { _.toString }  mkString ", "})"

}

case class ExtraInstallment(expressions: List[InstallmentEntity]) extends Installment(expressions) {

  override type Self = ExtraInstallment

  def copyEntity(): Self = copy()

  def copyEntities(expressions: List[InstallmentEntity]): Self = copy(expressions)

  def addEntity(entity: InstallmentEntity): Self = copyEntities(expressions ::: entity :: Nil)

  def addEntities(entities: List[InstallmentEntity]): Self = copyEntities(expressions ::: entities)

  // TODO: Add logic
  def applyEvent(event: LoanEvent) = ???

  override def toString = s"ExtraInstallment(${expressions map { _.toString }  mkString ", "})"

}
