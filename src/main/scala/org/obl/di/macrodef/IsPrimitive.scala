package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] class IsPrimitive[C <: Context](val context:C) {
  
  import context.universe._
  
  val boolean = typeOf[Boolean]
  val double = typeOf[Double]
  val string = typeOf[String]
  
  def apply(typ:Type):Boolean = {
    val r = typ <:< boolean || typ.weak_<:<(double) || typ <:< string
//    if (r) context.warning(context.enclosingPosition, s"$typ <:< boolean $r")
    r
  }
  
}