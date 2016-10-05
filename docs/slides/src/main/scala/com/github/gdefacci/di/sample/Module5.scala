package com.github.gdefacci.di.sample

object Module5A {
  val a = 10
  val b = "pippo"
}

object Module5B {
  def stringLenght(s:String) = s.length
}

class SetHold(val set:Set[Int])

object Module5C {
  import com.github.gdefacci.di.runtime.AllBindings

  val a = 15
  def createSetHold(a:AllBindings[Int]) = new SetHold(a.values.toSet)
}
