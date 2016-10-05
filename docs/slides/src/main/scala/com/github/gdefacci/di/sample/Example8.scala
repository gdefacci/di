package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example8 extends App {
  
  assert(IOC.get[String](Module8, Module8A) == "Foo Bar")
  
}