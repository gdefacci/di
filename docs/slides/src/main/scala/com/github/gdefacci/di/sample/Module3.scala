package com.github.gdefacci.di.sample

object Module3 {

  def createComp1(b:ModuleClass1) = new Comp1(144, b.text)
  
}
