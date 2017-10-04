package karazinscalausersgroup.typed.formula.numbers.number

import karazinscalausersgroup.typed.formula.numbers.Number.{DoubleNumber, IntNumber, LongNumber}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
  * @author Igor Wolkov
  */
object LongNumbersSpecification extends Properties("LongNumbersSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  // Arithmetic operations
  property("LongNumber instance") = forAll { long: Long =>
    LongNumber(long).v == long
  }

  property("LongNumber + IntNumber is LongNumber") = forAll { (l: Long, r: Int) =>
    LongNumber(l) + IntNumber(r) == LongNumber(l + r)
  }

  property("LongNumber + LongNumber is LongNumber") = forAll { (l: Long, r: Long) =>
    LongNumber(l) + LongNumber(r) == LongNumber(l + r)
  }

  property("LongNumber + DoubleNumber is DoubleNumber") = forAll { (l: Long, r: Double) =>
    LongNumber(l) + DoubleNumber(r) == DoubleNumber(l + r)
  }

  property("LongNumber * IntNumber is LongNumber") = forAll { (l: Long, r: Int) =>
    LongNumber(l) * IntNumber(r) == LongNumber(l * r)
  }

  property("LongNumber * LongNumber is LongNumber") = forAll { (l: Long, r: Long) =>
    LongNumber(l) * LongNumber(r) == LongNumber(l * r)
  }

  property("LongNumber * DoubleNumber is DoubleNumber") = forAll { (l: Long, r: Double) =>
    LongNumber(l) * DoubleNumber(r) == DoubleNumber(l * r)
  }

  property("LongNumber / IntNumber is DoubleNumber") = forAll { (l: Long, r: Int) =>
    (r != 0) ==> (LongNumber(l) / IntNumber(r) == DoubleNumber(l.toDouble / r.toDouble))
  }

  property("LongNumber / LongNumber is DoubleNumber") = forAll { (l: Long, r: Long) =>
    (r != 0) ==> (LongNumber(l) / LongNumber(r) == DoubleNumber(l.toDouble / r.toDouble))
  }

  property("LongNumber / DoubleNumber is DoubleNumber") = forAll { (l: Long, r: Double) =>
    (r != 0) ==> (LongNumber(l) / DoubleNumber(r) == DoubleNumber(l.toDouble / r))
  }

  // Logic operations
  // <
  property("LongNumber < IntNumber iff LongNumber value less than IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("LongNumber < LongNumber iff LongNumber value less than LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("LongNumber < DoubleNumber iff DoubleNumber value less than DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l < r) == (l.v < r.v)
  }

  // <=
  property("LongNumber <= IntNumber iff LongNumber value less or equals to IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("LongNumber <= LongNumber iff LongNumber value less or equals to LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("LongNumber <= DoubleNumber iff DoubleNumber value less or equals to DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  // ==
  property("LongNumber == IntNumber iff LongNumber value equals to IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("LongNumber == LongNumber iff LongNumber value equals to LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("LongNumber == DoubleNumber iff DoubleNumber value equals to DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l == r) == (l.v == r.v)
  }

  // !=
  property("LongNumber != IntNumber iff LongNumber value not equals to IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("LongNumber != LongNumber iff LongNumber value not equals to LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("LongNumber != DoubleNumber iff DoubleNumber value not equals to DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l != r) == (l.v != r.v)
  }

  // >
  property("LongNumber > IntNumber iff LongNumber value greater than IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("LongNumber > LongNumber iff LongNumber value greater than LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("LongNumber > DoubleNumber iff DoubleNumber value greater than DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l > r) == (l.v > r.v)
  }

  // >=
  property("LongNumber >= IntNumber iff LongNumber value greater or equals to IntNumber value") = forAll { (l: LongNumber, r: IntNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("LongNumber >= LongNumber iff LongNumber value greater or equals to LongNumber value") = forAll { (l: LongNumber, r: LongNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("LongNumber >= DoubleNumber iff LongNumber value greater or equals to DoubleNumber value") = forAll { (l: LongNumber, r: DoubleNumber) =>
    (l >= r) == (l.v >= r.v)
  }

}
