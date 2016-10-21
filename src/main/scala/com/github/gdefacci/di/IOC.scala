package com.github.gdefacci.di

import com.github.gdefacci.di.graph.Dependency

import scala.language.experimental.macros
object IOC {

  def get[T](modules: Any*): T = macro macrodef.IOCMacro.get[T]

  def getSource[T](modules: Any*): String = macro macrodef.IOCMacro.getSource[T]

  def graph[T](modules: Any*): List[Dependency] = macro macrodef.IOCMacro.graph[T]

}