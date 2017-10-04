package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula._
import karazinscalausersgroup.typed.formula.numbers.operations._

/**
  * @author Igor Wolkov
  */
object builder {

  implicit def `value builder`[V <: Value[Number]](implicit manifest: Manifest[V]): ExpressionBuilder[Number, V] =
    (list: List[Expression[Number]]) => `get value`[V](list)

  implicit def `+ builder`[L <: Expression[Number], R <: Expression[Number]](implicit
                                                                             lBuilder: ExpressionBuilder[Number, L],
                                                                             rBuilder: ExpressionBuilder[Number, R]
                                                                            ): ExpressionBuilder[Number, L + R] =
    (list: List[Expression[Number]]) => lBuilder.build(list) flatMap { left =>
      rBuilder.build(list) map { right =>
        left + right
      }
    }

  implicit def `- builder`[L <: Expression[Number], R <: Expression[Number]](implicit
                                                                             lBuilder: ExpressionBuilder[Number, L],
                                                                             rBuilder: ExpressionBuilder[Number, R]
                                                                            ): ExpressionBuilder[Number, L - R] =
    (list: List[Expression[Number]]) => lBuilder.build(list) flatMap { left =>
      rBuilder.build(list) map { right =>
        left - right
      }
    }

  implicit def `unary - builder`[E <: Expression[Number]](implicit
                                                    builder: ExpressionBuilder[Number, E]
                                                   ): ExpressionBuilder[Number, ~[E]] =
    (list: List[Expression[Number]]) => builder.build(list) map { expression =>
      -expression
    }

  implicit def `* builder`[L <: Expression[Number], R <: Expression[Number]](implicit
                                                                             lBuilder: ExpressionBuilder[Number, L],
                                                                             rBuilder: ExpressionBuilder[Number, R]
                                                                            ): ExpressionBuilder[Number, L * R] =
    (list: List[Expression[Number]]) => lBuilder.build(list) flatMap { left =>
      rBuilder.build(list) map { right =>
        left * right
      }
    }

  implicit def `/ builder`[L <: Expression[Number], R <: Expression[Number]](implicit
                                                                             lBuilder: ExpressionBuilder[Number, L],
                                                                             rBuilder: ExpressionBuilder[Number, R]
                                                                            ): ExpressionBuilder[Number, L / R] =
    (list: List[Expression[Number]]) => lBuilder.build(list) flatMap { left =>
      rBuilder.build(list) map { right =>
        left / right
      }
    }

  def `get value`[V : Manifest](list: List[Expression[Number]]): Option[V] = list collectFirst {
    case v: V => v
  }

  def build[E <: Expression[Number]](list: List[Value[Number]])(implicit
                                                                builder: ExpressionBuilder[Number, E]): Option[E] =
    builder.build(list)

}
