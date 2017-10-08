# Overview

Type Formula allows you write lazy formula in a type safe mode and calculate value when it needed. 

# Generic Formulas
Typed Formula provides functionality to build formulas for numeric calculations on top of Integers, Longs and Double wrapped into [Numbers](https://github.com/IgorWolkov/typed-formula/blob/master/src/main/scala/karazinscalausersgroup/typed/formula/numbers/Number.scala).

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
res0: Number = IntNumber(3.0)

scala> volume.v
res1: Number = DoubleNumber(6.0)

```





Please review test:
* [How can you build formulas and manipulate with formulas.](https://github.com/IgorWolkov/typed-formula/blob/master/src/test/scala/karazinscalausersgroup/typed/formula/numbers/OperationsSpecification.scala)
* [How build loan booking accounting calculations in 1 day.](https://github.com/IgorWolkov/typed-formula/blob/master/src/test/scala/karazinscalausersgroup/typed/formula/numbers/entities/CoveringSpecification.scala)