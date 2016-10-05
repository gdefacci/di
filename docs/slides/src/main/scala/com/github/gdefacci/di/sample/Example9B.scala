package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example9B extends App {
  
  val hf = IOC.get[MyService](Module9, Module9A)
  
  assert(hf.makeHose == House(Person("myself", 33)))
  
}