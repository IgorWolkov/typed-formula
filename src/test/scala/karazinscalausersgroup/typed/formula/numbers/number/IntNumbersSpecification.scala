package karazinscalausersgroup.typed.formula.numbers.number

import karazinscalausersgroup.typed.formula.numbers.Number.{DoubleNumber, IntNumber, LongNumber}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
  * @author Igor Wolkov
  */
object IntNumbersSpecification extends Properties("IntNumbersSpecification") {

  import karazinscalausersgroup.typed.formula.numbers.generators._

  // Arithmetic operations
  property("IntNumber instance") = forAll { int: Int =>
    IntNumber(int).v == int
  }

  property("IntNumber + IntNumber is IntNumber") = forAll { (l: Int, r: Int) =>
    IntNumber(l) + IntNumber(r) == IntNumber(l + r)
  }

  property("IntNumber + LongNumber is LongNumber") = forAll { (l: Int, r: Long) =>
    IntNumber(l) + LongNumber(r) == LongNumber(l + r)
  }

  property("IntNumber + DoubleNumber is DoubleNumber") = forAll { (l: Int, r: Double) =>
    IntNumber(l) + DoubleNumber(r) == DoubleNumber(l + r)
  }

  property("IntNumber * IntNumber is IntNumber") = forAll { (l: Int, r: Int) =>
    IntNumber(l) * IntNumber(r) == IntNumber(l * r)
  }

  property("IntNumber * LongNumber is LongNumber") = forAll { (l: Int, r: Long) =>
    IntNumber(l) * LongNumber(r) == LongNumber(l * r)
  }

  property("IntNumber * DoubleNumber is DoubleNumber") = forAll { (l: Int, r: Double) =>
    IntNumber(l) * DoubleNumber(r) == DoubleNumber(l * r)
  }

  property("IntNumber / IntNumber is DoubleNumber") = forAll { (l: Int, r: Int) =>
    (r != 0) ==> (IntNumber(l) / IntNumber(r) == DoubleNumber(l.toDouble / r.toDouble))
  }

  property("IntNumber / LongNumber is DoubleNumber") = forAll { (l: Int, r: Long) =>
    (r != 0) ==> (IntNumber(l) / LongNumber(r) == DoubleNumber(l.toDouble / r.toDouble))
  }

  property("IntNumber / DoubleNumber is DoubleNumber") = forAll { (l: Int, r: Double) =>
    (r != 0) ==> (IntNumber(l) / DoubleNumber(r) == DoubleNumber(l.toDouble / r.toDouble))
  }

  // Logic operations
  // <
  property("IntNumber < IntNumber iff IntNumber value less than IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("IntNumber < LongNumber iff IntNumber value less than LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l < r) == (l.v < r.v)
  }

  property("IntNumber < DoubleNumber iff DoubleNumber value less than DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l < r) == (l.v < r.v)
  }

  // <=
  property("IntNumber <= IntNumber iff IntNumber value less or equals to IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("IntNumber <= LongNumber iff IntNumber value less or equals to LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  property("IntNumber <= DoubleNumber iff DoubleNumber value less or equals to DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l <= r) == (l.v <= r.v)
  }

  // ==
  property("IntNumber == IntNumber iff IntNumber value equals to IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("IntNumber == LongNumber iff IntNumber value equals to LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l == r) == (l.v == r.v)
  }

  property("IntNumber == DoubleNumber iff DoubleNumber value equals to DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l == r) == (l.v == r.v)
  }

  // !=
  property("IntNumber != IntNumber iff IntNumber value not equals to IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("IntNumber != LongNumber iff IntNumber value not equals to LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l != r) == (l.v != r.v)
  }

  property("IntNumber != DoubleNumber iff DoubleNumber value not equals to DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l != r) == (l.v != r.v)
  }

  // >
  property("IntNumber > IntNumber iff IntNumber value greater than IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("IntNumber > LongNumber iff IntNumber value greater than LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l > r) == (l.v > r.v)
  }

  property("IntNumber > DoubleNumber iff DoubleNumber value greater than DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l > r) == (l.v > r.v)
  }

  // >=
  property("IntNumber >= IntNumber iff IntNumber value greater or equals to IntNumber value") = forAll { (l: IntNumber, r: IntNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("IntNumber >= LongNumber iff IntNumber value greater or equals to LongNumber value") = forAll { (l: IntNumber, r: LongNumber) =>
    (l >= r) == (l.v >= r.v)
  }

  property("IntNumber >= DoubleNumber iff IntNumber value greater or equals to DoubleNumber value") = forAll { (l: IntNumber, r: DoubleNumber) =>
    (l >= r) == (l.v >= r.v)
  }

}
