package karazinscalausersgroup.typed.formula

import scala.annotation.unchecked.uncheckedVariance


/**
  * @author Igor Wolkov
  */
trait Entity[+T, +Event_ <: Event[T]] extends Expression[T] {
  type Self <: Entity[T, Event_]

  def applyEvent(event: Event_ @uncheckedVariance): (Entity[T, Event_], Event_)
}

trait ValueEntity[+T, +Event_ <: Event[T]]
  extends Entity[T, Event_] with Value[T @uncheckedVariance]

trait FoldableEntity[+T, +Event_ <: Event[T], +Entity_ <: Entity[T @uncheckedVariance, Event_] ]
  extends Entity[T, Event_] with Foldable[T @uncheckedVariance, Entity_] {

  val expressions: List[Entity_]
  def copyEntity(): Self
  def copyEntities(expressions: List[Entity_ @uncheckedVariance]): Self
  def applyEvent(event: Event_ @uncheckedVariance): (Self, Event_)
}
