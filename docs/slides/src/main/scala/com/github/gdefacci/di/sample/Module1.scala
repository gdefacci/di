package com.github.gdefacci.di.sample

object Module1 {
  val a = 12
}

class ModuleClass1(val b:Boolean) {
  val text = s"value:$b"
}
