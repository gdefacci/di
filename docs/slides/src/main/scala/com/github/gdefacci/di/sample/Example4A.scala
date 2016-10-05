package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example4A extends App {

  val i1 = IOC.get[String](Module1, new ModuleClass1(true), Module3, Module4)
  
  assert(i1 == "value:true")
}