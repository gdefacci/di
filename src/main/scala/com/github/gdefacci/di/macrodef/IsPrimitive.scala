package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] class IsPrimitive[C <: Context](val context:C) {
  
  import context.universe._
  
  val TInt = typeOf[Int]
  val TByte = typeOf[Byte]
  val TShort = typeOf[Short]
  val TBoolean = typeOf[Boolean]
  val TDouble = typeOf[Double]
  val TFloat = typeOf[Float]
  val TString = typeOf[String]
  
  def apply(typ:Type):Boolean = {
    typ match {
      case x if x <:< TInt || x <:<  TString || x <:<  TBoolean || x <:< TByte || x <:<  TDouble || x <:<  TFloat || x <:<  TShort => true
      case _ => false
    }
  }
  
}