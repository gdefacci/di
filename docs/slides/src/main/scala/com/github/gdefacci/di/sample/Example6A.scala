package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example6A extends App {
  val i1 = IOC.get[Base](Module6A, Module6B)

  assert( i1 match {
    case Sub1(5, false, "5") => true
    case _ => false
  })
}

