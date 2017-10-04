package karazinscalausersgroup.typed.formula

import scala.annotation.unchecked.uncheckedVariance


/**
  * @author Igor Wolkov
  */
trait Limit[+T] {
  def limit: T
}

trait Event[+T] {
  type R <: Event[T]
  def withLimit(limit: T @uncheckedVariance): R with Limit[T]
}

trait EventApplication[+T, +Event_ <: Event[T]] {
  type Ent <: Entity[T, Event_ @uncheckedVariance]
  type Expr <: Expression[T]
}
