package karazinscalausersgroup.typed.formula

/**
  * @author Igor Wolkov
  */
trait ExpressionReplacer[T, E <: Expression[T]] {
  def replace[L <: List[Expression[T]]](replacement: E, list: L): L
}
