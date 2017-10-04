package karazinscalausersgroup.typed.formula.numbers.number

import karazinscalausersgroup.typed.formula.numbers.Number
import karazinscalausersgroup.typed.formula.numbers.Number.{DoubleNumber, IntNumber, LongNumber}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
  * @author Igor Wolkov
  */
object NumbersSpecification extends Properties("NumbersSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  property("Number from Int is IntNumber") = forAll { int: Int =>
    Number(int) == IntNumber(int)
  }

  property("Number from Long is LongNumber") = forAll { long: Long =>
    Number(long) == LongNumber(long)
  }

  property("Number from Double is DoubleNumber") = forAll { double: Double =>
    Number(double) == DoubleNumber(double)
  }

  property("Number + Number is Number") = forAll { (l: Number, r: Number) =>
    (l + r).isInstanceOf[Number]
  }

  property("Number - Number is Number") = forAll { (l: Number, r: Number) =>
    (l - r).isInstanceOf[Number]
  }

  property("Number * Number is Number") = forAll { (l: Number, r: Number) =>
    (l * r).isInstanceOf[Number]
  }

  property("Number / Number is Number") = forAll { (l: Number, r: Number) =>
    (l / r).isInstanceOf[Number]
  }

}
