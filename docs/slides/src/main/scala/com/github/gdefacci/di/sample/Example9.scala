package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example9 extends App {
  
  val hf = IOC.get[HouseFactory](Module9, Module9A)
  
  assert(hf.apply("name") == House(Person("name", 33)))
  
}