package org.obl.di.tests

import org.obl.di.runtime.AllBindings
import javax.inject.Named
import org.obl.di.runtime.Bind

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
  
  sealed trait Trait1
  class Trait1ImplA extends Trait1
  class Trait1ImplB extends Trait1
  
  object NModBind {
    
    val bindA = Bind[Trait1, Trait1ImplA]
    val bindB = Bind[Trait1, Trait1ImplB]

    def allTrait1(sq1: AllBindings[Trait1]):Seq[Trait1] = sq1.values
    
  }
  

}