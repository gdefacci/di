package org.obl.di.tests

import org.obl.di.runtime.AllBindings
import javax.inject.Named

object MultiModule {

  object Mod1 {

    val i1 = 1
    val i2 = 2

    val boolean = false

  }

  object Mod2 {

    val i3 = 3
  }

  object Mod3 {

    def intSeq(sq: AllBindings[Int]): Seq[Int] = sq.values

    def i455 = 3343
  }
  
  object NMod1 {
    
    @Named("a") val i1 = 1
    val i2 = 2
    
  }
  
  object NMod2 {
    
    @Named("a") val i1 = 5
    val i2 = 7
    
  }
  
  object NMod3 {

    def intSeq(sq1: AllBindings[Int], @Named("a") sq2: AllBindings[Int]): (Seq[Int], Seq[Int]) = sq1.values -> sq2.values

    def i455 = 3343
  }

}