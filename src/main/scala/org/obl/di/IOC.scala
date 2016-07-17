package org.obl.di

import language.experimental.macros

object IOC {

  def get[T](modules: Any*): T = macro macrodef.IOCMacro.get[T]

  def getSource[T](modules: Any*): String = macro macrodef.IOCMacro.getSource[T]
}