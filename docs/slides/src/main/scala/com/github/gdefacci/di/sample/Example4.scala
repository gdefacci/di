package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example4 extends App {

  // Error: more than one instance of com.github.gdefacci.di.sample.Comp1 : 
  //        Module3.createComp1, Module4.createCompInstance
  
  // val i1 = IOC.get[Comp1](Module1, new ModuleClass1(true), Module3, Module4)

}