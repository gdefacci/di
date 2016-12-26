package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] case class Kind(id: Id, scope: DagScope)

private[di] case class Kinds(ids: Set[Id], scope: DagScope)

object Kinds {
  def default = Kinds(Set(Kind.default.id), Kind.default.scope)
}

private[di] object Kind {
  val default = Kind(Global, DefaultScope)
  val derived = Kind(Derived, DefaultScope)
}

private[di] sealed trait Id

private[di] case object Global extends Id
private[di] case object Derived extends Id

private[di] case class WithName(name: String) extends Id

private[di] case class WithQualifier(qualifierName: String, values: Map[String, Any]) extends Id

private[di] sealed trait DagScope

private[di] case object DefaultScope extends DagScope

private[di] case object SingletonScope extends DagScope

private[di] trait KindProvider[C <: Context] {
  val context: C


  def apply(sym: context.Symbol): Kinds

}

private[di] class DefaultKindProvider[C <: Context](val context: C) extends KindProvider[C] {

  import context.universe._

  private def showAnnotationInfo(ann: Annotation): String = {
    s"""type ${ann.tree.tpe}
    args ${ann.tree.children}
    value ${annotationStringAttribute(ann, "value")}"""
  }

  private val javaxInjectNamed = typeOf[javax.inject.Named]
  private val javaxInjectSingleton = typeOf[javax.inject.Singleton]
  private val javaxInjectQualifier = typeOf[javax.inject.Qualifier]

  private def annotationStringAttribute(annotation: Annotation, attr: String): Option[String] = {
    annotation.tree.children.tail.collectFirst {
      case arg @ AssignOrNamedArg(id: Ident, Literal(Constant(v: String))) if id.name.decodedName.toString == attr =>
        v
    }
  }

  private def isAnnotated(annotationTpe: Type, typeOfAnnotation: Type) = {
    annotationTpe.typeSymbol.annotations.exists(ann => ann.tree.tpe =:= typeOfAnnotation)
  }

  private def annotationId(annotation: Annotation): Option[Id] = {
    val annotationTpe: Type = annotation.tree.tpe
    if (annotationTpe =:= javaxInjectNamed) {
      val annName = annotationStringAttribute(annotation, "value").getOrElse {
        context.abort(context.enclosingPosition, "value attribute is mandatory for javax.injectNamed")
      }
      Some(WithName(annName))
    } else if (isAnnotated(annotationTpe, javaxInjectQualifier)) {
      annotation.tree.children match {
        case Nil => throw new RuntimeException("annotation macro erro")
        case Select(_, name) :: rest =>
          val constr = annotation.tree.tpe.members.find( m => m.isConstructor).get
          val pars = constr.asMethod.paramLists.flatten
          val parsMap = pars.zip(rest).map { case (par, Literal(Constant(a)))   =>
            par.name.toString -> a
          }
          Some(WithQualifier(annotationTpe.typeSymbol.fullName.toString, parsMap.toMap))
      }
    } else {
      None
    }
  }

  private def annotationScope(annotation: Annotation): Option[DagScope] = {
    if (annotation.tree.tpe =:= javaxInjectSingleton) {
      Some(SingletonScope)
    } else
      None
  }

  private def getAnnotations(m:Symbol):List[Annotation] = {
    /** start - workaround to https://issues.scala-lang.org/browse/SI-7424 */
    m.typeSignature
    m.annotations.foreach(_.tree.tpe)
    /** end   - workaround to https://issues.scala-lang.org/browse/SI-7424 */
    m.annotations
  }
  
  def apply(sym: context.Symbol): Kinds = {
    Option(sym).map { sym =>

      val annotations = if (sym.isMethod) {
        val mthd = sym.asMethod
        (if (mthd.isAccessor) getAnnotations(mthd.accessed) else Nil) ++ getAnnotations(mthd)
      } else if (sym.isParameter) {
        sym.annotations
      } else
        Nil

      val z: (Set[Id], Option[DagScope], Option[Boolean]) = (Set.empty, None, None)
      val (ids, optScope, isItem) = annotations.foldLeft(z) { (acc, annotation) =>
        val (ids, scope, isItem) = acc
        val nscope = scope -> annotationScope(annotation) match {
          case (Some(_), Some(_)) => context.abort(context.enclosingPosition, "more than one scope annotation ")
          case (x, y) => y.orElse(x)
        }
        (ids ++ annotationId(annotation).toSet, nscope, isItem)
      }

      Kinds(if (ids.nonEmpty) ids else Set(Global), optScope.getOrElse(DefaultScope))
    } getOrElse {
      Kinds.default
    }
  }

}