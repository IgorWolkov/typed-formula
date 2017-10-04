package karazinscalausersgroup.typed.formula.numbers


import karazinscalausersgroup.typed.formula.numbers.Number.{DoubleNumber, IntNumber, LongNumber, Zero}
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.{Expression, Foldable, Value}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck._

import scala.util.Random

/**
  * @author Igor Wolkov
  */
object generators {

  case class IntValue(v: IntNumber) extends Value[Number]
  case class LongValue(v: LongNumber) extends Value[Number]
  case class DoubleValue(v: DoubleNumber) extends Value[Number]
  case class UnexpectedValue(v: Number) extends Value[Number]

  case class PreviousIntValue(v: IntNumber) extends Value[Number]
  case class PreviousLongValue(v: LongNumber) extends Value[Number]
  case class PreviousDoubleValue(v: DoubleNumber) extends Value[Number]
  case class PreviousUnexpectedValue(v: Number) extends Value[Number]

  case class CurrentIntValue(v: IntNumber) extends Value[Number]
  case class CurrentLongValue(v: LongNumber) extends Value[Number]
  case class CurrentDoubleValue(v: DoubleNumber) extends Value[Number]
  case class CurrentUnexpectedValue(v: Number) extends Value[Number]

  case class NextIntValue(v: IntNumber) extends Value[Number]
  case class NextLongValue(v: LongNumber) extends Value[Number]
  case class NextDoubleValue(v: DoubleNumber) extends Value[Number]
  case class NextUnexpectedValue(v: Number) extends Value[Number]

  case class FirstConstantValue(v: Number) extends Value[Number]
  case class SecondConstantValue(v: Number) extends Value[Number]

  // Numbers
  // We don't need big numbers because of unexpected formula building
  lazy val genIntNumber = for {value <- arbitrary[Short]} yield IntNumber(value.toInt)
  lazy val genLongNumber = for {value <- arbitrary[Short]} yield LongNumber(value.toLong)
  lazy val genDoubleNumber = for {value <- arbitrary[Short]} yield DoubleNumber(value.toDouble)
  lazy val genNumber = oneOf(genIntNumber, genLongNumber, genDoubleNumber)

  // Expressions
  lazy val genValue = for {number <- arbitrary[Number]} yield Value(number)
  lazy val `unary - gen` = for {expression <- genExpression} yield -expression
  lazy val `+ gen` = for {left <- genExpression; right <- genExpression} yield left + right
  lazy val `- gen` = for {left <- genExpression; right <- genExpression} yield left - right
  lazy val `* gen` = for {left <- genExpression; right <- genExpression} yield left * right
  lazy val `/ gen` = for {left <- genExpression; right <- genExpression if right.v != Zero} yield left / right
  // Use the trick with several `genValue` to achieve the same probability for complex and simple expressions
  lazy val genExpression: Gen[Expression[Number]] =
    lzy(frequency((1, `unary - gen`), (1, `+ gen`), (1, `- gen`), (1, `* gen`), (1, `/ gen`), (10, genValue)))

  // Values
  lazy val genIntValue: Gen[IntValue] = for {number <- genIntNumber} yield IntValue(number)
  lazy val genLongValue: Gen[LongValue] = for {number <- genLongNumber} yield LongValue(number)
  lazy val genDoubleValue: Gen[DoubleValue] = for {number <- genDoubleNumber} yield DoubleValue(number)
  lazy val genUnexpectedValue: Gen[UnexpectedValue] = for {
    number <- oneOf(arbitrary[IntNumber], arbitrary[LongNumber], arbitrary[DoubleNumber])
  } yield UnexpectedValue(number)

  lazy val genPreviousIntValue: Gen[PreviousIntValue] = for {number <- genIntNumber} yield PreviousIntValue(number)
  lazy val genPreviousLongValue: Gen[PreviousLongValue] = for {number <- genLongNumber} yield PreviousLongValue(number)
  lazy val genPreviousDoubleValue: Gen[PreviousDoubleValue] = for {number <- genDoubleNumber} yield PreviousDoubleValue(number)
  lazy val genPreviousUnexpectedValue: Gen[PreviousUnexpectedValue] = for {
    number <- oneOf(arbitrary[IntNumber], arbitrary[LongNumber], arbitrary[DoubleNumber])
  } yield PreviousUnexpectedValue(number)

  lazy val genCurrentIntValue: Gen[CurrentIntValue] = for {number <- genIntNumber} yield CurrentIntValue(number)
  lazy val genCurrentLongValue: Gen[CurrentLongValue] = for {number <- genLongNumber} yield CurrentLongValue(number)
  lazy val genCurrentDoubleValue: Gen[CurrentDoubleValue] = for {number <- genDoubleNumber} yield CurrentDoubleValue(number)
  lazy val genCurrentUnexpectedValue: Gen[CurrentUnexpectedValue] = for {
    number <- oneOf(arbitrary[IntNumber], arbitrary[LongNumber], arbitrary[DoubleNumber])
  } yield CurrentUnexpectedValue(number)

  lazy val genNextIntValue: Gen[NextIntValue] = for {number <- genIntNumber} yield NextIntValue(number)
  lazy val genNextLongValue: Gen[NextLongValue] = for {number <- genLongNumber} yield NextLongValue(number)
  lazy val genNextDoubleValue: Gen[NextDoubleValue] = for {number <- genDoubleNumber} yield NextDoubleValue(number)
  lazy val genNextUnexpectedValue: Gen[NextUnexpectedValue] = for {
    number <- oneOf(arbitrary[IntNumber], arbitrary[LongNumber], arbitrary[DoubleNumber])
  } yield NextUnexpectedValue(number)

  lazy val genFirstConstantValue: Gen[FirstConstantValue] = for {number <- genNumber} yield FirstConstantValue(number)
  lazy val genSecondConstantValue: Gen[SecondConstantValue] = for {number <- genNumber} yield SecondConstantValue(number)

  // Foldable
  // Non zero values to avoid a mess with division
  lazy val genPreviousFoldable1: Gen[Foldable[Number, Value[Number]]] =
    for {
      intValue <- genPreviousIntValue
      if intValue.v != Zero
      longValue <- genPreviousLongValue
      if longValue.v != Zero
      doubleValue <- genPreviousDoubleValue
      if doubleValue.v != Zero
    } yield
      createFoldable1(Random.shuffle(intValue :: longValue :: doubleValue :: Nil))

  // Non zero values to avoid a mess with division
  lazy val genCurrentFoldable1: Gen[Foldable[Number, Value[Number]]] =
    for {
      intValue <- genCurrentIntValue
      if intValue.v != Zero
      longValue <- genCurrentLongValue
      if longValue.v != Zero
      doubleValue <- genCurrentDoubleValue
      if doubleValue.v != Zero
    } yield
      createFoldable1(Random.shuffle(intValue :: longValue :: doubleValue :: Nil))

  // Non zero values to avoid a mess with division
  lazy val genNextFoldable1: Gen[Foldable[Number, Value[Number]]] =
    for {
      intValue <- genNextIntValue
      if intValue.v != Zero
      longValue <- genNextLongValue
      if longValue.v != Zero
      doubleValue <- genNextDoubleValue
      if doubleValue.v != Zero
    } yield
      createFoldable1(Random.shuffle(intValue :: longValue :: doubleValue :: Nil))

  lazy val genFoldable2: Gen[Foldable[Number, Foldable[Number, Value[Number]]]] =
    for {
      previous1 <- genPreviousFoldable1
      previous2 <- genPreviousFoldable1
      current <- genCurrentFoldable1
      next1 <- genNextFoldable1
      next2 <- genNextFoldable1
    } yield
      createFoldable2(Random.shuffle(previous1 :: previous2 :: current :: next1 :: next2 :: Nil))


  implicit lazy val arbitraryIntNumber: Arbitrary[IntNumber] = Arbitrary(genIntNumber)
  implicit lazy val arbitraryLongNumber: Arbitrary[LongNumber] = Arbitrary(genLongNumber)
  implicit lazy val arbitraryDoubleNumber: Arbitrary[DoubleNumber] = Arbitrary(genDoubleNumber)
  implicit lazy val arbitraryNumber: Arbitrary[Number] = Arbitrary(genNumber)
  implicit lazy val arbitraryValue: Arbitrary[Value[Number]] = Arbitrary(genValue)
  implicit lazy val arbitraryExpression: Arbitrary[Expression[Number]] = Arbitrary(genExpression)

  implicit lazy val arbitraryIntValue: Arbitrary[IntValue] = Arbitrary(genIntValue)
  implicit lazy val arbitraryLongValue: Arbitrary[LongValue] = Arbitrary(genLongValue)
  implicit lazy val arbitraryDoubleValue: Arbitrary[DoubleValue] = Arbitrary(genDoubleValue)
  implicit lazy val arbitraryUnexpectedValue: Arbitrary[UnexpectedValue] = Arbitrary(genUnexpectedValue)

  implicit lazy val arbitraryFirstConstantValue: Arbitrary[FirstConstantValue] = Arbitrary(genFirstConstantValue)
  implicit lazy val arbitrarySecondConstantValue: Arbitrary[SecondConstantValue] = Arbitrary(genSecondConstantValue)

  implicit lazy val arbitraryFoldable2: Arbitrary[Foldable[Number, Foldable[Number, Value[Number]]]] = Arbitrary(genFoldable2)

  private def createFoldable1(values: List[Value[Number]]): Foldable[Number, Value[Number]] =
    new AbstractFoldable[Value[Number]](
      values,
      (expression, value) => expression + value
    ) {
      // TODO: Fix. Should return entity with updated values
    }

  private def createFoldable2(values: List[Foldable[Number, Value[Number]]]): Foldable[Number, Foldable[Number, Value[Number]]] =
    new AbstractFoldable[Foldable[Number, Value[Number]]](
      values,
      (expression, value) => expression + value
    ) {
      // TODO: Fix. Should return entity with updated expressions
    }
}
