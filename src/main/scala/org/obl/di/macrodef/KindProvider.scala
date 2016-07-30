package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] case class Kind(id: Id, scope: DagScope)

private[di] case class Kinds(ids: Set[Id], scope: DagScope)

object Kinds {
  def default = Kinds(Set(Kind.default.id), Kind.default.scope)
}

private[di] object Kind {
  def default = Kind(Global, DefaultScope)
}

private[di] sealed trait Id

private[di] case object Global extends Id

private[di] case class WithName(name: String) extends Id

private[di] case class WithQualifier(qualifierName: String, values: Map[String, Any]) extends Id

private[di] sealed trait DagScope

private[di] case object DefaultScope extends DagScope

private[di] case object SingletonScope extends DagScope

private[di] trait KindProvider[C <: Context] {
  val context: C

  import context.universe._

  def apply(sym: context.Symbol): Kinds

}

private[di] class DefaultKindProvider[C <: Context](val context: C) extends KindProvider[C] {

  import context.universe._

  private def showAnnotationInfo(ann: Annotation): String = {
    s"""type ${ann.tree.tpe}
    args ${ann.tree.children}
    value ${annotationStringAttribute(ann, "value")}"""
  }

  private lazy val javaxInjectNamed = typeOf[javax.inject.Named]
  private lazy val javaxInjectSingleton = typeOf[javax.inject.Singleton]
  private lazy val javaxInjectQualifier = typeOf[javax.inject.Qualifier]

  private def annotationStringAttribute(annotation: Annotation, attr: String): Option[String] = {
    annotation.tree.children.tail.collectFirst {
      case arg@AssignOrNamedArg(id: Ident, Literal(Constant(v: String))) if id.name.decodedName.toString == attr =>
        v
    }
  }

  private def isAnnotated(annotationTpe: Type, typeOfAnnotation: Type) = {
    annotationTpe.typeSymbol.annotations.exists(ann => ann.tree.tpe =:= typeOfAnnotation)
  }

  private def annotationIds(annotations: List[Annotation]): Set[Id] = {
    val r = annotations.toSet.flatMap { annotation: Annotation =>
      val annotationTpe: Type = annotation.tree.tpe 
      if (annotationTpe =:= javaxInjectNamed) {
        val annName = annotationStringAttribute(annotation, "value").getOrElse {
          context.abort(context.enclosingPosition, "value attribute is mandatory for javax.injectNamed")
        }
        Set[Id](WithName(annName))
      } else if (isAnnotated(annotationTpe, javaxInjectQualifier)) {
        annotation.tree.children match {
          case Nil => throw new RuntimeException("annotation macro erro")
          case Select(_, name) :: rest =>
            val parsMap = rest.collect {
              case arg@AssignOrNamedArg(id: Ident, Literal(Constant(v))) =>
                id.name.decodedName.toString -> v
            }
            Set[Id](WithQualifier(name.decodedName.toString, parsMap.toMap))
        }
      } else {
//        val itemType = typeOf[org.obl.di.Item[_]]
////        context.warning(annotation.tree.pos, "annotation  =" + annotation.tree.children.mkString(" -- "))
//        if (annotation.tree.tpe <:< itemType){
//          context.warning(annotation.tree.pos, "annotation  =" + annotation.tree.tpe.typeArgs.mkString(", "))
//        }

        Set.empty[Id]
      }
    }
    if (r.nonEmpty) r else Set(Global)
  }

  private def annotationScope(annotations: List[Annotation]): Option[DagScope] = {
    annotations.flatMap { annotation: Annotation =>
      if (annotation.tree.tpe =:= javaxInjectSingleton) {
        //context.warning(annotation.tree.pos, "SingletonScope --")
        Seq[DagScope](SingletonScope)
      } else
        Seq.empty[DagScope]
    }.toSeq match {
      case Seq() => None
      case hd +: Seq() => Some(hd)
      case _ => context.abort(context.enclosingPosition, "more than one scope annotation ")
    }
  }

  def apply(sym: context.Symbol): Kinds = {
    Option(sym).map { sym =>

      val annotations = if (sym.isMethod) {
        val mthd = sym.asMethod
        (if (mthd.isAccessor) mthd.accessed.annotations else Nil) ++ mthd.annotations
      } else if (sym.isParameter) {
        sym.annotations
      } else
        Nil


      Kinds(annotationIds(annotations), annotationScope(annotations).getOrElse(DefaultScope))

    } getOrElse {
      Kinds.default
    }
  }

}
