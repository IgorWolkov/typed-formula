package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula._

/**
  * @author Igor Wolkov
  */
object replacer {

  implicit def `value replacer`[V <: Expression[Number]](implicit manifest: Manifest[V]): ExpressionReplacer[Number, V] =
    new ExpressionReplacer[Number, V] {
      def replace[L <: List[Expression[Number]]](replacement: V, list: L): L =
        `replace value`[V, L](replacement, list)
    }

  def `replace value`[V <: Expression[Number] : Manifest, L <: List[Expression[Number]]](replacement: V, list: L): L =
    (list map {
      case _: V                      => replacement
      case other: Expression[Number] => other
    }).asInstanceOf[L]

  def replace[V <: Expression[Number], L <: List[Expression[Number]]](replacement: V, list: L)
                                      (implicit replacer: ExpressionReplacer[Number, V]): L =
    replacer.replace(replacement, list)

}
