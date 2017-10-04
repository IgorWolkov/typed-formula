package karazinscalausersgroup.typed.formula.numbers

import karazinscalausersgroup.typed.formula.{Expression, _}
import shapeless._

import scala.annotation.unchecked.uncheckedVariance
import scala.language.reflectiveCalls

/**
  * @author Igor Wolkov
  */
object covering {

  trait NumberEventApplication extends EventApplication[Number, Event[Number]]

  type `with value of`[Ent0 <: Entity[Number, Event[Number]], Expr0 <: Expression[Number]] =
    NumberEventApplication {
      type Ent = Ent0
      type Expr = Expr0
    }

  type BigFold = FoldableEntity[Number, Event[Number], FoldableEntity[Number, Event[Number], ValueEntity[Number, Event[Number]]]]

  trait Cover[T] {
    val processors: List[Processors[Expression[Number], NumberEventApplication]]

    def applyEvent[
      E <: Event[Number],
      VE <: ValueEntity[Number, E],
      FE0 <: FoldableEntity[Number, E, VE],
      FE1 <: FoldableEntity[Number, E, FE0] { def copyEntities(expressions: List[FE0]): FE1 },
      FE2 <: FoldableEntity[Number, E, FE1] { def copyEntities(expressions: List[FE1]): FE2 }
    ]
    (entity: FE2, event: E, defaults: Seq[Value[Number]]): (FE2, E) = {

      // TODO: Get rif of var
      var eventAcc: E = event

      val internalEntities =
        entity.expressions map { internalEntity =>

          val resultedInternalEntity =
            processors.foldLeft(internalEntity.expressions) { (expressions, processors) =>
              processors match {
                case Processors(expressionReplacer, expressionBuilder, eventApplicationBuilder) =>

                  val entity = eventApplicationBuilder.build(expressions) collect {
                    case entity: Entity[Number, Event[Number]] => entity
                  }

                  val expression = expressionBuilder.build(expressions ++ defaults)

                  if (entity.nonEmpty && expression.nonEmpty) {

                    val (newEntity, newEvent: E) = entity.get.applyEvent(eventAcc withLimit expression.get.v)

                    eventAcc = newEvent

                    expressionReplacer.replace(newEntity, expressions)
                  } else {
                    expressions
                  }
              }
            }

          internalEntity.copyEntities(resultedInternalEntity)
        }

        val res = entity.copyEntities(internalEntities)

      (res, eventAcc)
    }

  }

  object Cover {

    def apply[T](implicit cover: Cover[T]): Cover[T] = cover

    def create[
      FE2 <: FoldableEntity[Number, Event[Number], FoldableEntity[Number, Event[Number], ValueEntity[Number, Event[Number]]]],
      NEA <: NumberEventApplication
    ]
    (implicit
     foldable2: FE2,
     defaults: Seq[Value[Number]],
     expressionReplacer: ExpressionReplacer[Number, NEA#Ent],
     expressionBuilder: ExpressionBuilder[Number, NEA#Expr],
     entityBuilder: ExpressionBuilder[Number, NEA#Ent]
    ): Cover[NEA] =
      new Cover[NEA] {
        val processors = List(Processors(expressionReplacer, expressionBuilder, entityBuilder))
      }

    def create[T, Replacing <: Expression[Number], NEA <: NumberEventApplication]
    (list: List[Processors[Replacing, NumberEventApplication]]): Cover[T] =
      new Cover[T] {
        val processors = list
      }

    implicit def eventCover[
      FE2 <: FoldableEntity[Number, Event[Number], FoldableEntity[Number, Event[Number], ValueEntity[Number, Event[Number]]]],
      NEA <: NumberEventApplication
    ]
    (implicit
     foldable2: FE2,
     defaults: Seq[Value[Number]],
     expressionReplacer: ExpressionReplacer[Number, NEA#Ent],
     expressionBuilder: ExpressionBuilder[Number, NEA#Expr],
     entityBuilder: ExpressionBuilder[Number, NEA#Ent]
    ): Cover[NEA] =
      Cover.create[FE2, NEA](foldable2, defaults, expressionReplacer, expressionBuilder, entityBuilder)

    implicit def singleCover[NEA <: NumberEventApplication]
    (implicit cover: Cover[NEA]): Cover[NEA :: HNil] =
      Cover.create[NEA :: HNil, Expression[Number], NEA](cover.processors)

    implicit def hListCover[H <: NumberEventApplication, T <: HList]
    (implicit hCover: Cover[H], tCover: Cover[T]): Cover[H :: T] =
      Cover.create[H :: T, Expression[Number], H](hCover.processors ::: tCover.processors)

  }

  case class Processors[
    +Replacing <: Expression[Number],
    +NEA <: NumberEventApplication
  ]
  (expressionReplacer: ExpressionReplacer[Number, Replacing@uncheckedVariance],
   expressionBuilder: ExpressionBuilder[Number, NEA#Expr@uncheckedVariance],
   entityBuilder: ExpressionBuilder[Number, NEA#Ent@uncheckedVariance])
}
