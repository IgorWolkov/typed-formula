package karazinscalausersgroup.typed.formula

/**
  * @author Igor Wolkov
  */
trait ExpressionBuilder[T, E <: Expression[T]] {
  def build(list: List[Expression[T]]): Option[E]
}