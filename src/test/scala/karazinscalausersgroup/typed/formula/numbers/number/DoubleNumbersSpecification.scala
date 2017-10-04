package karazinscalausersgroup.typed.formula.numbers.number

import karazinscalausersgroup.typed.formula.numbers.Number.{DoubleNumber, IntNumber, LongNumber}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
  * @author Igor Wolkov
  */
object DoubleNumbersSpecification extends Properties("DoubleNumbersSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  // Arithmetic operations
  property("DoubleNumber instance") = forAll { double: Double =>
    DoubleNumber(double).v == double
  }

  property("DoubleNumber + IntNumber is DoubleNumber") = forAll { (l: Double, r: Int) =>
    DoubleNumber(l) + IntNumber(r) == DoubleNumber(l + r)
  }

  property("DoubleNumber + LongNumber is DoubleNumber") = forAll { (l: Double, r: Long) =>
    DoubleNumber(l) + LongNumber(r) == DoubleNumber(l + r)
  }

  property("DoubleNumber + DoubleNumber is DoubleNumber") = forAll { (l: Double, r: Double) =>
    DoubleNumber(l) + DoubleNumber(r) == DoubleNumber(l + r)
  }

  property("DoubleNumber * IntNumber is DoubleNumber") = forAll { (l: Double, r: Int) =>
    DoubleNumber(l) * IntNumber(r) == DoubleNumber(l * r)
  }

  property("DoubleNumber * LongNumber is DoubleNumber") = forAll { (l: Double, r: Long) =>
    DoubleNumber(l) * LongNumber(r) == DoubleNumber(l * r)
  }

  property("DoubleNumber * DoubleNumber is DoubleNumber") = forAll { (l: Double, r: Double) =>
    DoubleNumber(l) * DoubleNumber(r) == DoubleNumber(l * r)
  }

  property("DoubleNumber / IntNumber is DoubleNumber") = forAll { (l: Double, r: Int) =>
    (r != 0) ==> (DoubleNumber(l) / IntNumber(r) == DoubleNumber(l / r))
  }

  property("DoubleNumber / LongNumber is DoubleNumber") = forAll { (l: Double, r: Long) =>
    (r != 0) ==> (DoubleNumber(l) / LongNumber(r) == DoubleNumber(l / r))
  }

  property("DoubleNumber / DoubleNumber is DoubleNumber") = forAll { (l: Double, r: Double) =>
    (r != 0) ==> (DoubleNumber(l) / DoubleNumber(r) == DoubleNumber(l / r))
  }

  // Logic operations
  // <
  property("DoubleNumber < IntNumber iff DoubleNumber value less than IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("DoubleNumber < LongNumber iff DoubleNumber value less than LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("DoubleNumber < DoubleNumber iff DoubleNumber value less than DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l < r) == (l.v < r.v)
  }

  // <=
  property("DoubleNumber <= IntNumber iff DoubleNumber value less or equals to IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("DoubleNumber <= LongNumber iff DoubleNumber value less or equals to LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("DoubleNumber <= DoubleNumber iff DoubleNumber value less or equals to DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  // ==
  property("DoubleNumber == IntNumber iff DoubleNumber value equals to IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("DoubleNumber == LongNumber iff DoubleNumber value equals to LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("DoubleNumber == DoubleNumber iff DoubleNumber value equals to DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l == r) == (l.v == r.v)
  }

  // !=
  property("DoubleNumber != IntNumber iff DoubleNumber value not equals to IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("DoubleNumber != LongNumber iff DoubleNumber value not equals to LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("DoubleNumber != DoubleNumber iff DoubleNumber value not equals to DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l != r) == (l.v != r.v)
  }

  // >
  property("DoubleNumber > IntNumber iff DoubleNumber value greater than IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("DoubleNumber > LongNumber iff DoubleNumber value greater than LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("DoubleNumber > DoubleNumber iff DoubleNumber value greater than DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l > r) == (l.v > r.v)
  }

  // >=
  property("DoubleNumber >= IntNumber iff DoubleNumber value greater or equals to IntNumber value") = forAll { (l: DoubleNumber, r: IntNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("DoubleNumber >= LongNumber iff DoubleNumber value greater or equals to LongNumber value") = forAll { (l: DoubleNumber, r: LongNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("DoubleNumber >= DoubleNumber iff DoubleNumber value greater or equals to DoubleNumber value") = forAll { (l: DoubleNumber, r: DoubleNumber) =>
    (l >= r) == (l.v >= r.v)
  }

}
