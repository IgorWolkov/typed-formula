package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula.numbers.Number._
import karazinscalausersgroup.typed.formula.numbers.builder._
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.{Expression, ExpressionBuilder, Value}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

import scala.util.Random


/**
  * @author Igor Wolkov
  */
object BuilderSpecification extends Properties("BuilderSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  property("Prove build of IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue](intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue](longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue](doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Refute build of UnexpectedValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, unexpectedValue: UnexpectedValue) =>
    `refute builder`[UnexpectedValue](unexpectedValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Negation
  property("Prove build of negation: -IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[~[IntValue]](-intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of negation: -LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[~[LongValue]](-longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Refute build of negation: -UnexpectedValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue, unexpectedValue: UnexpectedValue) =>
    `refute builder`[~[UnexpectedValue]](-unexpectedValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of negation: -DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[~[DoubleValue]](-doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Sum
  property("Prove build of sum: IntValue + LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue + LongValue](intValue + longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of sum: IntValue + DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue + DoubleValue](intValue + doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of sum: LongValue + IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue + IntValue](longValue + intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of sum: LongValue + DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue + DoubleValue](longValue + doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of sum: DoubleValue + IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue + IntValue](doubleValue + intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of sum: DoubleValue + LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue + LongValue](doubleValue + longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Difference
  property("Prove build of difference: IntValue - LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue - LongValue](intValue - longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of difference: IntValue - DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue - DoubleValue](intValue - doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of difference: LongValue - IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue - IntValue](longValue - intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of difference: LongValue - DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue - DoubleValue](longValue - doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of difference: DoubleValue - IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue - IntValue](doubleValue - intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of difference: DoubleValue - LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue - LongValue](doubleValue - longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Product
  property("Prove build of product: IntValue * LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue * LongValue](intValue * longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of product: IntValue * DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[IntValue * DoubleValue](intValue * doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of product: LongValue * IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue * IntValue](longValue * intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of product: LongValue * DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[LongValue * DoubleValue](longValue * doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of product: DoubleValue * IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue * IntValue](doubleValue * intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of product: DoubleValue * LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[DoubleValue * LongValue](doubleValue * longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Quotient
  property("Prove build of quotient: IntValue / LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (longValue.v != Zero) ==> `prove builder`[IntValue / LongValue](intValue / longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of quotient: IntValue / DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (doubleValue.v != Zero) ==> `prove builder`[IntValue / DoubleValue](intValue / doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of quotient: LongValue / IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (intValue.v != Zero) ==> `prove builder`[LongValue / IntValue](longValue / intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of quotient: LongValue / DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (doubleValue.v != Zero) ==> `prove builder`[LongValue / DoubleValue](longValue / doubleValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of quotient: DoubleValue / IntValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (intValue.v != Zero) ==> `prove builder`[DoubleValue / IntValue](doubleValue / intValue, intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of quotient: DoubleValue / LongValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (longValue.v != Zero) ==> `prove builder`[DoubleValue / LongValue](doubleValue / longValue, intValue :: longValue :: doubleValue :: Nil)
  }

  // Mixed
  property("Prove build of mix: (-IntValue + -LongValue) * -DoubleValue") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[(~[IntValue] + ~[LongValue]) * ~[DoubleValue]](((-intValue) + (-longValue)) * (-doubleValue), intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of mix: -IntValue * ( -LongValue + -DoubleValue) ") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    `prove builder`[~[IntValue] * (~[LongValue] + ~[DoubleValue])]((-intValue) * ((-longValue) + (-doubleValue)), intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of mix: -(-(IntValue + LongValue) / DoubleValue)") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (doubleValue.v != Zero) ==>
      `prove builder`[~[~[(IntValue + LongValue)] / DoubleValue]](-(-(intValue + longValue) / doubleValue), intValue :: longValue :: doubleValue :: Nil)
  }

  property("Prove build of mix: -(IntValue / -(LongValue + DoubleValue))") = forAll { (intValue: IntValue, longValue: LongValue, doubleValue: DoubleValue) =>
    (longValue.v != Zero && doubleValue.v != Zero) ==>
      `prove builder`[~[IntValue / ~[(LongValue + DoubleValue)]]](-(intValue / -(longValue + doubleValue)), intValue :: longValue :: doubleValue :: Nil)
  }

  private def `prove builder`[E <: Expression[Number]](expected: E, values: List[Value[Number]])(
    implicit builder: ExpressionBuilder[Number, E]): Boolean = {

    builder.build(Random.shuffle(values)) match {
      case Some(result) => result.isInstanceOf[E] && expected.v == result.v
      case None => false
    }
  }

  private def `refute builder`[E <: Expression[Number]](expected: E, values: List[Value[Number]])(
    implicit builder: ExpressionBuilder[Number, E]): Boolean =
    !`prove builder`(expected, values)(builder)
}
