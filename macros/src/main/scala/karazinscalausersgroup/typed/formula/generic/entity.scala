package karazinscalausersgroup.typed.formula.generic

import org.scalameta.logger

import scala.meta._
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.immutable.Seq

@compileTimeOnly("@entity annotation should have been removed by compiler but was not")
class entity extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def trace(s: => String): Unit = {
      val predicate = sys.props.get("karazinscalausersgroup.typed.formula.generic.entity.trace").isDefined
      if (predicate) logger.debug(s)
    }

    def isCaseClass(c: Defn.Class): Boolean = {
      c.mods exists {
        case mod"case" => true
        case _         => false
      }
    }

    def generate(c: Defn.Class): Seq[Stat] = {
      type Matcher = PartialFunction[Stat, Unit]

      val stats = c.templ.stats.getOrElse(Nil)

      val expressionsType = c.ctor.paramss.flatten
        .collectFirst {
          case Term.Param(_, Term.Name("expressions"), Some(Type.Apply(Type.Name("List"), tpe :: Nil)), _) =>
            tpe
        }
        .getOrElse {
          abort("'expressions: List[X]' field is not defined in a case class constructor")
        }

      def addIfNotExist(matcher: Matcher, stat: => Stat): Option[Stat] = {
        if (stats.exists(matcher.isDefinedAt)) None else Some(stat)
      }

      val selfTypeMatcher: Matcher = { case Defn.Type(_, Type.Name("Self"), _, _) => }

      val copyEntityMatcher: Matcher = { case Defn.Def(_, Term.Name("copyEntity"), _, _, _, _) => }

      val copyEntitiesMatcher: Matcher = { case Defn.Def(_, Term.Name("copyEntities"), _, _, _, _) => }

      def selfTypeDefinition = {
        q"override type Self = ${c.name}"
      }

      def copyEntityDefinition = {
        q"override def copyEntity(): Self = copy()"
      }

      def copyEntitiesDefinition = {
        q"override def copyEntities(expressions: List[$expressionsType]): Self = copy(expressions = expressions)"
      }

      List(
        addIfNotExist(selfTypeMatcher, selfTypeDefinition),
        addIfNotExist(copyEntityMatcher, copyEntityDefinition),
        addIfNotExist(copyEntitiesMatcher, copyEntitiesDefinition)
      ).flatten
    }

    defn match {
      case c: Defn.Class if isCaseClass(c) =>
        val generated = generate(c)

        val adjustedClass = c.copy(
          templ = c.templ.copy(
            stats = Some(c.templ.stats.getOrElse(Nil) ++ generated)
          )
        )

        trace(adjustedClass.syntax)

        adjustedClass

      case Term.Block((c: Defn.Class) :: (o: Defn.Object) :: Nil) if isCaseClass(c) =>
        val generated = generate(c)

        val adjustedClass = c.copy(
          templ = c.templ.copy(
            stats = Some(c.templ.stats.getOrElse(Nil) ++ generated)
          )
        )

        trace(adjustedClass.syntax)

        Term.Block(adjustedClass :: o :: Nil)

      case _ =>
        abort("@entity must annotate a case class")
    }
  }

}