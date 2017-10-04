package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula.numbers.replacer._
import karazinscalausersgroup.typed.formula.{ExpressionReplacer, Value}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.language.postfixOps
import scala.util.Random

/**
  * @author Igor Wolkov
  */
object ReplacerSpecification extends Properties("ReplacerSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  property("Prove replacement of IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, replacement: IntValue) =>
    `prove replacer`[IntValue](replacement, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove replacement of LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, replacement: LongValue) =>
    `prove replacer`[LongValue](replacement, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove replacement of DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, replacement: DoubleValue) =>
    `prove replacer`[DoubleValue](replacement, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Refute replacement of UnexpectedValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, replacement: UnexpectedValue) =>
    `refute replacer`[UnexpectedValue](replacement, intValue :: longValue :: doubleValue :: Nil)
  }

  private def `prove replacer`[V <: Value[Number]](replacement: V, values: List[Value[Number]])
                                                  (implicit replacer: ExpressionReplacer[Number, V]): Boolean =
    replacer.replace(replacement, Random.shuffle(values)) filter (_ == replacement) match {
      case _ :: Nil => true
      case Nil => false
    }

  private def `refute replacer`[V <: Value[Number]](replacement: V, values: List[Value[Number]])
                                                   (implicit replacer: ExpressionReplacer[Number, V]): Boolean =
    !`prove replacer`(replacement, values)(replacer)

}
