package com.github.gdefacci.di.tests


object SingleSamples {

  class ClR(i:Int, clr: => ClR) 
  
  object ModCyc {
    
    def createClR(i:Int, clr: => ClR) = new ClR(i, clr) 
  
  }
  
  
}