package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula.numbers.Number.Zero
import karazinscalausersgroup.typed.formula.numbers.builder._
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.{Expression, Foldable, Value}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

import scala.language.postfixOps

/**
  * @author Igor Wolkov
  */
object OperationsSpecification extends Properties("OperationsSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  // Operations on Values
  property("Value of negative Value is equals to negative value of Value") = forAll { value: Value[Number] =>
    (-value).v == -value.v
  }

  property("Value of sum of two Values is equals to sum of values of two Values") = forAll { (left: Value[Number], right: Value[Number]) =>
    (left + right).v == left.v + right.v
  }

  property("Value of difference of two Values is equals to difference of values of two Values") = forAll { (left: Value[Number], right: Value[Number]) =>
    (left - right).v == left.v - right.v
  }

  property("Value of product of two Values is equals to product of values of two Values") = forAll { (left: Value[Number], right: Value[Number]) =>
    (left * right).v == left.v * right.v
  }

  property("Value of quotient of two Values is equals to quotient of values of two Values") = forAll { (left: Value[Number], right: Value[Number]) =>
    (right.v != Zero) ==> ((left / right).v == left.v / right.v)
  }

  // Operations on Expressions
  property("Value of negative Expression is equals to negative value of Expression") = forAll { expression: Expression[Number] =>
    (-expression).v == -expression.v
  }

  property("Value of sum of two Expressions is equals to sum of values of two Expressions") = forAll { (left: Expression[Number], right: Expression[Number]) =>
    (left + right).v == left.v + right.v
  }

  property("Value of difference of two Expressions is equals to difference of values of two Expressions") = forAll { (left: Expression[Number], right: Expression[Number]) =>
    (left - right).v == left.v - right.v
  }

  property("Value of product of two Expressions is equals to product of values of two Expressions") = forAll { (left: Expression[Number], right: Expression[Number]) =>
    (left * right).v == left.v * right.v
  }

  property("Value of quotient of two Expressions is equals to quotient of values of two Expressions") = forAll { (left: Expression[Number], right: Expression[Number]) =>
    (right.v != Zero) ==> ((left / right).v == left.v / right.v)
  }

  // Operations on folds
  property("Value of sum of PreviousIntValue is equals to sum of values of PreviousIntValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[PreviousIntValue].v
    val expected = `value of`[PreviousIntValue]

    result == expected
  }

  property("Value of sum of PreviousLongValue is equals to sum of values of PreviousLongValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[PreviousLongValue].v
    val expected = `value of`[PreviousLongValue]

    result == expected
  }

  property("Value of sum of PreviousDoubleValue is equals to sum of values of PreviousDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[PreviousDoubleValue].v
    val expected = `value of`[PreviousDoubleValue]

    result == expected
  }

  property("Value of sum of CurrentIntValue is equals to sum of values of CurrentIntValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[CurrentIntValue].v
    val expected = `value of`[CurrentIntValue]

    result == expected
  }

  property("Value of sum of CurrentLongValue is equals to sum of values of CurrentLongValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[CurrentLongValue].v
    val expected = `value of`[CurrentLongValue]

    result == expected
  }

  property("Value of sum of CurrentDoubleValue is equals to sum of values of CurrentDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[CurrentDoubleValue].v
    val expected = `value of`[CurrentDoubleValue]

    result == expected
  }

  property("Value of sum of NextIntValue is equals to sum of values of NextIntValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[NextIntValue].v
    val expected = `value of`[NextIntValue]

    result == expected
  }

  property("Value of sum of NextLongValue is equals to sum of values of NextLongValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[NextLongValue].v
    val expected = `value of`[NextLongValue]

    result == expected
  }

  property("Value of sum of NextDoubleValue is equals to sum of values of NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = Σ[NextDoubleValue].v
    val expected = `value of`[NextDoubleValue]

    result == expected
  }

  property("Value of sum of sums of PreviousIntValue, PreviousLongValue and PreviousDoubleValue is equals to sum of sums of values of PreviousIntValue, PreviousLongValue and PreviousDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val previousIntValues = entities[PreviousIntValue]
    val previousLongValues = entities[PreviousLongValue]
    val previousDoubleValues = entities[PreviousDoubleValue]

    val expected = (previousIntValues(0).v + previousLongValues(0).v + previousDoubleValues(0).v) +
                   (previousIntValues(1).v + previousLongValues(1).v + previousDoubleValues(1).v)

    val result = Σ[PreviousIntValue + PreviousLongValue + PreviousDoubleValue].v

    result == expected
  }

  property("Value of sum of subtractions of PreviousIntValue, PreviousLongValue and PreviousDoubleValue is equals to sum of subtractions of values of PreviousIntValue, PreviousLongValue and PreviousDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val previousIntValues = entities[PreviousIntValue]
    val previousLongValues = entities[PreviousLongValue]
    val previousDoubleValues = entities[PreviousDoubleValue]

    val expected = (previousIntValues(0).v - previousLongValues(0).v - previousDoubleValues(0).v) +
                   (previousIntValues(1).v - previousLongValues(1).v - previousDoubleValues(1).v)

    val result = Σ[PreviousIntValue - PreviousLongValue - PreviousDoubleValue].v

    result == expected
  }


  property("Value of sum of products of PreviousIntValue, PreviousLongValue and PreviousDoubleValue is equals to sum of products of values of PreviousIntValue, PreviousLongValue and PreviousDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val previousIntValues = entities[PreviousIntValue]
    val previousLongValues = entities[PreviousLongValue]
    val previousDoubleValues = entities[PreviousDoubleValue]

    val expected = (previousIntValues(0).v * previousLongValues(0).v * previousDoubleValues(0).v) +
                   (previousIntValues(1).v * previousLongValues(1).v * previousDoubleValues(1).v)

    val result = Σ[PreviousIntValue * PreviousLongValue * PreviousDoubleValue].v

    result == expected
  }

  property("Value of sum of divisions of PreviousIntValue, PreviousLongValue and PreviousDoubleValue is equals to sum of divisions of values of PreviousIntValue, PreviousLongValue and PreviousDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val previousIntValues = entities[PreviousIntValue]
    val previousLongValues = entities[PreviousLongValue]
    val previousDoubleValues = entities[PreviousDoubleValue]

    val expected = (previousIntValues(0).v / previousLongValues(0).v / previousDoubleValues(0).v) +
                   (previousIntValues(1).v / previousLongValues(1).v / previousDoubleValues(1).v)

    val result = Σ[PreviousIntValue / PreviousLongValue / PreviousDoubleValue].v

    result == expected
  }

  property("Value of sum of sums of CurrentIntValue, CurrentLongValue and CurrentDoubleValue is equals to sum of sums of values of CurrentIntValue, CurrentLongValue and CurrentDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val currentIntValues = entities[CurrentIntValue]
    val currentLongValues = entities[CurrentLongValue]
    val currentDoubleValues = entities[CurrentDoubleValue]

    val expected = currentIntValues(0).v + currentLongValues(0).v + currentDoubleValues(0).v
    val result = Σ[CurrentIntValue + CurrentLongValue + CurrentDoubleValue].v

    result == expected
  }

  property("Value of sum of subtractions of CurrentIntValue, CurrentLongValue and CurrentDoubleValue is equals to sum of subtractions of values of CurrentIntValue, CurrentLongValue and CurrentDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val currentIntValues = entities[CurrentIntValue]
    val currentLongValues = entities[CurrentLongValue]
    val currentDoubleValues = entities[CurrentDoubleValue]

    val expected = currentIntValues(0).v - currentLongValues(0).v - currentDoubleValues(0).v
    val result = Σ[CurrentIntValue - CurrentLongValue - CurrentDoubleValue].v

    result == expected
  }

  property("Value of sum of products of CurrentIntValue, CurrentLongValue and CurrentDoubleValue is equals to sum of products of values of CurrentIntValue, CurrentLongValue and CurrentDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val currentIntValues = entities[CurrentIntValue]
    val currentLongValues = entities[CurrentLongValue]
    val currentDoubleValues = entities[CurrentDoubleValue]

    val expected = currentIntValues(0).v * currentLongValues(0).v * currentDoubleValues(0).v
    val result = Σ[CurrentIntValue * CurrentLongValue * CurrentDoubleValue].v

    result == expected
  }

  property("Value of sum of divisions of CurrentIntValue, CurrentLongValue and CurrentDoubleValue is equals to sum of products of divisions of CurrentIntValue, CurrentLongValue and CurrentDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val currentIntValues = entities[CurrentIntValue]
    val currentLongValues = entities[CurrentLongValue]
    val currentDoubleValues = entities[CurrentDoubleValue]

    val expected = currentIntValues(0).v / currentLongValues(0).v / currentDoubleValues(0).v
    val result = Σ[CurrentIntValue / CurrentLongValue / CurrentDoubleValue].v

    result == expected
  }

  property("Value of sum of sums of NextIntValue, NextLongValue and NextDoubleValue is equals to sum of sums of values of NextIntValue, NextLongValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val nextIntValues = entities[NextIntValue]
    val nextLongValues = entities[NextLongValue]
    val nextDoubleValues = entities[NextDoubleValue]

    val expected = (nextIntValues(0).v + nextLongValues(0).v + nextDoubleValues(0).v) +
                   (nextIntValues(1).v + nextLongValues(1).v + nextDoubleValues(1).v)

    val result = Σ[NextIntValue + NextLongValue + NextDoubleValue].v

    result == expected
  }

  property("Value of sum of subtractions of NextIntValue, NextLongValue and NextDoubleValue is equals to sum of subtractions of values of NextIntValue, NextLongValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val nextIntValues = entities[NextIntValue]
    val nextLongValues = entities[NextLongValue]
    val nextDoubleValues = entities[NextDoubleValue]

    val expected = (nextIntValues(0).v - nextLongValues(0).v - nextDoubleValues(0).v) +
                   (nextIntValues(1).v - nextLongValues(1).v - nextDoubleValues(1).v)

    val result = Σ[NextIntValue - NextLongValue - NextDoubleValue].v

    result == expected
  }

  property("Value of sum of products of NextIntValue, NextLongValue and NextDoubleValue is equals to sum of products of values of NextIntValue, NextLongValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val nextIntValues = entities[NextIntValue]
    val nextLongValues = entities[NextLongValue]
    val nextDoubleValues = entities[NextDoubleValue]

    val expected = (nextIntValues(0).v * nextLongValues(0).v * nextDoubleValues(0).v) +
                   (nextIntValues(1).v * nextLongValues(1).v * nextDoubleValues(1).v)

    val result = Σ[NextIntValue * NextLongValue * NextDoubleValue].v

    result == expected
  }

  property("Value of sum of divisions of NextIntValue, NextLongValue and NextDoubleValue is equals to sum of divisions of values of NextIntValue, NextLongValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val nextIntValues = entities[NextIntValue]
    val nextLongValues = entities[NextLongValue]
    val nextDoubleValues = entities[NextDoubleValue]

    val expected = (nextIntValues(0).v / nextLongValues(0).v / nextDoubleValues(0).v) +
                   (nextIntValues(1).v / nextLongValues(1).v / nextDoubleValues(1).v)

    val result = Σ[NextIntValue / NextLongValue / NextDoubleValue].v

    result == expected
  }

  property("Value of sums of PreviousIntValue, CurrentLongValue and NextDoubleValue is equals to sums of values of PreviousIntValue, CurrentLongValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val result = (Σ[PreviousIntValue] + Σ[CurrentLongValue] + Σ[NextDoubleValue]).v
    val expected = `value of`[PreviousIntValue] + `value of`[CurrentLongValue] + `value of`[NextDoubleValue]

    result == expected
  }

  property("Value of sums of sum of product of PreviousIntValue and PreviousLongValue and sum of sum of CurrentLongValue and CurrentDoubleValue and sum of division of NextIntValue and NextDoubleValue is equals to sums of sum of product of values of PreviousIntValue and PreviousLongValue and sum of sum of values of  CurrentLongValue and CurrentDoubleValue and sum of division of values of NextIntValue and NextDoubleValue") = forAll { implicit foldable: Foldable[Number, Foldable[Number, Value[Number]]] =>
    implicit val defaults: Seq[Value[Number]] = Seq.empty

    val previousIntValues = entities[PreviousIntValue]
    val previousLongValues = entities[PreviousLongValue]

    val currentLongValues = entities[CurrentLongValue]
    val currentDoubleValues = entities[CurrentDoubleValue]

    val nextIntValues = entities[NextIntValue]
    val nextDoubleValues = entities[NextDoubleValue]

    val previous = (previousIntValues(0).v * previousLongValues(0).v) +
                   (previousIntValues(1).v * previousLongValues(1).v)

    val current = currentLongValues(0).v + currentDoubleValues(0).v

    val next = (nextIntValues(0).v / nextDoubleValues(0).v) +
               (nextIntValues(1).v / nextDoubleValues(1).v)

    val result =
      (Σ[PreviousIntValue * PreviousLongValue] +
       Σ[CurrentLongValue + CurrentDoubleValue] +
       Σ[NextIntValue / NextDoubleValue]).v

    val expected = previous + current + next

    result == expected
  }

  property("Value of sum of sum of product of FirstConstantValue and PreviousIntValue and product of SecondConstantValue and sum of CurrentLongValue is equals to sum of sum of product of values of FirstConstantValue and PreviousIntValue and product of value of SecondConstantValue and sum of values of CurrentLongValue") = forAll { (foldable: Foldable[Number, Foldable[Number, Value[Number]]], firstConstant: FirstConstantValue, secondConstant: SecondConstantValue) =>
    implicit val f: Foldable[Number, Foldable[Number, Value[Number]]] = foldable
    implicit val defaults: Seq[Value[Number]] = Seq(firstConstant, secondConstant)

    val result = (Σ[FirstConstantValue * PreviousIntValue] + secondConstant * Σ[CurrentLongValue]).v

    val previousIntValues = entities[PreviousIntValue]

    val expected = (firstConstant.v * previousIntValues(0).v) +
                   (firstConstant.v * previousIntValues(1).v) +
                   (secondConstant.v * `value of`[CurrentLongValue])

    result == expected
  }

  private def entities[T <: Value[Number]](implicit
                                           foldable: Foldable[Number, Foldable[Number, Value[Number]]],
                                           manifest: Manifest[T]): List[T] =
    foldable.expressions.flatMap {
      _.expressions collect { case v: T => v }
    }

  private def `value of`[T <: Value[Number]](implicit
                                             foldable: Foldable[Number, Foldable[Number, Value[Number]]],
                                             manifest: Manifest[T]): Number =
    entities[T](foldable, manifest).foldLeft(Zero)( (acc: Number, v: T) => acc + v.v )

}
