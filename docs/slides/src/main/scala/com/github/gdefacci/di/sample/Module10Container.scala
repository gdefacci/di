package com.github.gdefacci.di.sample

import com.github.gdefacci.di.runtime.ModulesContainer

case class User(id: Int, name: String, admin: Boolean)

object Module10Container extends ModulesContainer {
  val mod1 = Module10A
  val mod2 = Module10B
  val mod3 = Module10C
}
