package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example3 extends App {
  val i1 = IOC.get[Comp1](new ModuleClass1(true), Module3)
  
  assert(i1.a == 144)
  assert(i1.b == "value:true")
  
}
