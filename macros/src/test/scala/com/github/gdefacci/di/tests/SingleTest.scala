package com.github.gdefacci.di.tests

import com.github.gdefacci.di.IOC
import org.scalatest.FunSuite
import com.github.gdefacci.di.runtime.AllBindings

object SingleTest extends App {
  
    import samples3._

    val mff = IOC.get[(Int => String) => MyFactory]()
    
    assert("12" == mff( _.toString).factory(12))
    
}