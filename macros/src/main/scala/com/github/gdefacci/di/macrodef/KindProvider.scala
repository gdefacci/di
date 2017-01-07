package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] sealed trait DagScope

private[di] case object DefaultScope extends DagScope

private[di] case object ApplicationScope extends DagScope

private[di] class DagScopeProvider[C <: Context](val context: C) {

  import context.universe._

  private val applicationScopeAnnotation = typeOf[com.github.gdefacci.di.runtime.ApplicationScope]
  
  private def annotationScope(annotation: Annotation): Option[DagScope] = {
    if (annotation.tree.tpe =:= applicationScopeAnnotation) {
      Some(ApplicationScope)
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
  
  def apply(sym: context.Symbol): DagScope = {
    Option(sym).flatMap { sym =>

      val annotations = if (sym.isMethod) {
        val mthd = sym.asMethod
        (if (mthd.isAccessor) getAnnotations(mthd.accessed) else Nil) ++ getAnnotations(mthd)
      } else if (sym.isParameter) {
        sym.annotations
      } else
        Nil

      val z: Option[DagScope] = None
      val (optScope) = annotations.foldLeft(z) { (scope, annotation) =>
        val nscope = scope -> annotationScope(annotation) match {
          case (Some(_), Some(_)) => context.abort(context.enclosingPosition, "more than one scope annotation ")
          case (x, y) => y.orElse(x)
        }
        nscope
      }

      optScope
    } getOrElse {
      DefaultScope
    }
  }

}
