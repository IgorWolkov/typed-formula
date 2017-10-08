# Overview

Type Formula allows you write lazy formula in a type safe mode and calculate value when it needed. 

# Generic Formulas
*typed-formula* provides functionality to build formulas for numeric calculations on top of Integers, Longs and Double wrapped into [Numbers](https://github.com/IgorWolkov/typed-formula/blob/master/src/main/scala/karazinscalausersgroup/typed/formula/numbers/Number.scala).

Let's define simple formulas to calculate area and a volume of a figure.
Firstly define measurements.

```scala
import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.{Number, Value}
import karazinscalausersgroup.typed.formula.numbers.operations._


scala> val length = Value[Number](1)
length: Value[Number] = Expression(IntNumber(1))

scala> val width = Value[Number](3)
width: Value[numbers.Number] = Expression(IntNumber(3))

scala> val height = Value[Number](2.0)
height: Value[numbers.Number] = Expression(DoubleNumber(2.0))

```

Now we can easily define formulas

```scala
scala> val area = length * width
area: Value[Number] * Value[Number] = (Expression(IntNumber(1)) * Expression(IntNumber(3)))

scala> val volume = area * height
volume: Value[Number] * Value[Number] * Value[Number] = ((Expression(IntNumber(1)) * Expression(IntNumber(3))) * Expression(DoubleNumber(2.0)))
```

and calculate values 

```scala
scala> area.v
res0: Number = IntNumber(3)

scala> volume.v
res1: Number = DoubleNumber(6.0)

```

# Custom formulas
Let's rewrite the example with custom classes for formula terms

```scala
import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.{Number, Value}
import karazinscalausersgroup.typed.formula.numbers.operations._


case class Length(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Width(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Height(value: Double) extends Value[Number] {
  def v = Number(value)
}

scala>  val length = Length(1)
length: Length = Expression(IntNumber(1))

scala> val width = Width(3)
width: Width = Expression(IntNumber(3))

scala> val height = Height(2.0)
height: Height = Expression(DoubleNumber(2.0))

```

Now we can define formulas with custom term
```scala
scala> val area = length * width
area: Length * Width = (Expression(IntNumber(1)) * Expression(IntNumber(3)))

scala> val volume = area * height
volume: Length * Width * Height = ((Expression(IntNumber(1)) * Expression(IntNumber(3))) * Expression(DoubleNumber(2.0)))

```

and calculate values 

```scala
scala> area.v
res0: Number = IntNumber(3)

scala> volume.v
res1: Number = DoubleNumber(6.0)

```

# Supported operations
In current version only operations for Numbers are supported.

## Linear operations
Let's define two variables
```scala
import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.{Number, Value}
import karazinscalausersgroup.typed.formula.numbers.operations._


scala> val a = Value[Number](3)
a: Value[Number] = Expression(IntNumber(3))

scala> val b = Value[Number](5)
b: Value[Number] = Expression(IntNumber(5))
```

*typed-formula* supports following linear operation: 
### addition
```scala

scala> a + b
res1: Value[Number] + Value[Number] = (Expression(IntNumber(3)) + Expression(IntNumber(5)))

```

### subtraction
```scala
scala> a - b
res2: Value[Number] - Value[Number] = (Expression(IntNumber(3)) - Expression(IntNumber(5)))

```

### unary minus
Unlike `unary_` definitions for methods, it is impossible to overload types. That's why `~` (tilde) represents unary minus on type level. However it's possible to use `-` as unary minus with values.
```scala
scala> -a
res3: ~[Value[Number]] = -Expression(IntNumber(3)))

```

### multiplication
```scala
scala> a * b
res4: Value[Number] * Value[Number] = (Expression(IntNumber(3)) * Expression(IntNumber(5)))

```

### division

```scala
scala> a / b
res9: Value[numbers.Number] / Value[numbers.Number] = (Expression(IntNumber(3)) / Expression(IntNumber(5)))

```

## "Series" operations and formula builder
### Formula builder

Let's get to the example with measurements

```scala
import karazinscalausersgroup.typed.formula.numbers.Number.Zero
import karazinscalausersgroup.typed.formula.numbers.builder._
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.numbers.{Number, Val


case class Length(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Width(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Height(value: Double) extends Value[Number] {
  def v = Number(value)
}

scala>  val length = Length(1)
length: Length = Expression(IntNumber(1))

scala> val width = Width(3)
width: Width = Expression(IntNumber(3))

scala> val height = Height(2.0)
height: Height = Expression(DoubleNumber(2.0))

```

Let's pack all measurements into a list.

```scala
scala> val measurements = length :: width :: height :: Nil
measurements: List[Value[numbers.Number] with Product with Serializable{def v: Product with Serializable with Number{def v: AnyVal; type N >: Double with Int <: AnyVal}}] = List(Expression(IntNumber(1)), Expression(IntNumber(3)), Expression(DoubleNumber(2.0)))

```

Now we can build formulas on the fly. Take into account that `build` works only for list of [`Value`](https://github.com/IgorWolkov/typed-formula/blob/master/src/main/scala/karazinscalausersgroup/typed/formula/Expression.scala#L15)s. 
```scala
scala> val areaOpt = build[Length * Width](measurements)
areaOpt: Option[Length * Width] = Some((Expression(IntNumber(1)) * Expression(IntNumber(3))))


scala> val volumeOpt = build[Length * Width * Height](measurements)
volumeOpt: Option[Length * Width * Height] = Some(((Expression(IntNumber(1)) * Expression(IntNumber(3))) * Expression(DoubleNumber(2.0))))

```

If it's possible to build formula `build` returns `Some` formula otherwise it returns `None`. For some reasons it may be preferable to fail on compile time if it is impossible to build a formula, but this soft check is necessary for `Σ` operation.

## Σ
Suppose there is a series of measurements. We can calculate additive characteristics on this series.  

```scala
import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.{Number, Value}
import karazinscalausersgroup.typed.formula.numbers.operations._


case class Length(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Width(value: Int) extends Value[Number] {
  def v = Number(value)
}

case class Height(value: Double) extends Value[Number] {
  def v = Number(value)
}
```

Define a foldable entity for collection of measurements
```scala
case class Measurements(expressions: List[Value[Number]]) extends Foldable[Number, Value[Number]] {
  lazy val op: (Expression[Number], Value[Number]) => Expression[Number] = (acc, value) => acc + value
  lazy val initial = Value(Zero)
}

scala> val measurements1 = Measurements(Length(1) :: Width(3) :: Height(2.0) :: Nil)
measurements1: Measurements = Expression(DoubleNumber(6.0))

scala> val measurements2 = Measurements(Length(4) :: Width(6) :: Height(5.0) :: Nil)
measurements2: Measurements = Expression(DoubleNumber(15.0))

scala> val measurements3 = Measurements(Length(7) :: Width(9) :: Height(8.0) :: Nil)
measurements3: Measurements = Expression(DoubleNumber(24.0))
```

And define a foldable entity for series of measurements
```scala
case class Series(expressions: List[Measurements]) extends Foldable[Number, Measurements] {
  lazy val op: (Expression[Number], Measurements) => Expression[Number] = (acc, value) => acc + value
  lazy val initial = Value(Zero)
}

scala> implicit val series: Series = Series(measurements1 :: measurements2 :: measurements3 :: Nil)
series: Series = Expression(DoubleNumber(45.0))
```

It is also needed to add `implicit` default entities. This option will be described later. 
```scala
scala> implicit val defaults: List[Value[Number]] = Nil
defaults: List[Value[Number]] = List()
```

Now we can calculate some additive characteristics.
The sum of all lengths:
```scala
scala> Σ[Length].v
res0: IntNumber = IntNumber(12)
```

The sum of all volumes:
```scala
scala> Σ[Length * Width * Height].v
res1: DoubleNumber = DoubleNumber(630.0)
```



Please review test:
* [How can you build formulas and manipulate with formulas.](https://github.com/IgorWolkov/typed-formula/blob/master/src/test/scala/karazinscalausersgroup/typed/formula/numbers/OperationsSpecification.scala)
* [How build loan booking accounting calculations in 1 day.](https://github.com/IgorWolkov/typed-formula/blob/master/src/test/scala/karazinscalausersgroup/typed/formula/numbers/entities/CoveringSpecification.scala)