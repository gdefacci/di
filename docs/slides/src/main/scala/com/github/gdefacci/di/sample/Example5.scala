package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example5 extends App {
  import com.github.gdefacci.di.runtime.AllBindings
  
  val sh = IOC.get[SetHold](Module5A, Module5B,  Module5C)
  
  assert(sh.set == Set(5,10,15))
  
  val i1 = IOC.get[AllBindings[Int]](Module5A, Module5B,  Module5C)
  
  assert(i1.values.toSet == Set(5,10,15))
}
