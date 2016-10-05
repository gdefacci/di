package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

class Comp1(val a:Int, val b:String)

object Example2 extends App {
  val i1 = IOC.get[Comp1](Module1, new ModuleClass1(true))
  
  assert(i1.a == 12)
  assert(i1.b == "value:true")
  
  val i2 = IOC.get[Comp1](Module1, new ModuleClass1(true))
  
  assert(i1 != i2)
}
