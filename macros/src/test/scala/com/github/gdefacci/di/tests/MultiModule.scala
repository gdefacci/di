package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.{AllBindings, Bind, ModulesContainer}

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
  
    
  sealed trait Trait1
  class Trait1ImplA extends Trait1
  class Trait1ImplB extends Trait1
  
  object NModBind {
    
    val bindA = Bind[Trait1, Trait1ImplA]
    val bindB = Bind[Trait1, Trait1ImplB]

    def allTrait1(sq1: AllBindings[Trait1]):Seq[Trait1] = sq1.values
    
  }
  
  object ModBag extends ModulesContainer {
    val m1 = Mod1
    val m2 = Mod2
    def m3 = Mod3
  }

  case class MultiIncompleteCl(s:String, i:Int)
  
  object MultiIncomplete {
    
    val str = "pippo"
    
    def seqOfInt(mics:AllBindings[MultiIncompleteCl]):Seq[Int] = {
      mics.values.map(_.i)
    }
    
    val mci = Bind[MultiIncompleteCl,MultiIncompleteCl]
    
  }
  
}