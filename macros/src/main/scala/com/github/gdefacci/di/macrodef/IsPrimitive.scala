package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] class IsPrimitive[C <: Context](val context:C) {
  
  import context.universe._
  
  private val TInt = typeOf[Int]
  private val TByte = typeOf[Byte]
  private val TShort = typeOf[Short]
  private val TBoolean = typeOf[Boolean]
  private val TDouble = typeOf[Double]
  private val TFloat = typeOf[Float]
  private val TString = typeOf[String]
  
  def apply(typ:Type):Boolean = {
    typ match {
      case x if x <:< TInt || x <:<  TString || x <:<  TBoolean || x <:< TByte || x <:<  TDouble || x <:<  TFloat || x <:<  TShort => true
      case _ => false
    }
  }
  
}