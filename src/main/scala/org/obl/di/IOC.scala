package org.obl.di

import language.experimental.macros
import org.obl.di.graph.Dependency

object IOC {

  def get[T](modules: Any*): T = macro macrodef.IOCMacro.get[T]

  def getSource[T](modules: Any*): String = macro macrodef.IOCMacro.getSource[T]

  def graph[T](modules: Any*): List[Dependency] = macro macrodef.IOCMacro.graph[T]

}