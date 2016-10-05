package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example1 extends App {
  assert(IOC.get[Int](Module1, new ModuleClass1(true)) == 12)
  
  val tup = IOC.get[(Int,String)](Module1, new ModuleClass1(true))
  assert(tup == (12, "value:true"))
}
