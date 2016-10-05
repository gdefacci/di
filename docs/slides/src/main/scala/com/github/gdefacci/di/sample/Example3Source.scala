package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example3Source extends App {
  val ex2Src = IOC.getSource[Comp1](Module1, new ModuleClass1(true))
  val ex3Src = IOC.getSource[Comp1](new ModuleClass1(true), Module3)
  
  println(s"Example3 source:\n$ex2Src")
  println(s"Example2 source:\n$ex3Src")
  
}
