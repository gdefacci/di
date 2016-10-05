package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example9C extends App {
  
  val hf = IOC.get[MyServiceA](Module9, Module9C)
  
  assert(hf.makeHose == House(Person("myself", 33)))
  
}