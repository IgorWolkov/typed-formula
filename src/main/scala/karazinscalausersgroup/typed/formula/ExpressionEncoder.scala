package karazinscalausersgroup.typed.formula

/**
  * @author Igor Wolkov
  */
trait ExpressionEncoder[T, E <: Expression[T]] {
  def encode(e: E): String
}
