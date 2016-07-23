package org.obl.di.tests

import javax.inject.Qualifier

import scala.annotation.StaticAnnotation

@Qualifier
case class Qual1(val value:Int, val name:String) extends StaticAnnotation
