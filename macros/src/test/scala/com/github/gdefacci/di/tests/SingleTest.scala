package com.github.gdefacci.di.tests

import com.github.gdefacci.di.IOC
import org.scalatest.FunSuite
import com.github.gdefacci.di.runtime.AllBindings

object SingleTest extends App {
  
    import tagSamples._
    
    val lst = IOC.get[Option[String]](tagSamples.moduleH)
    
    println(lst)
    println(IOC.getSource[Option[String]](tagSamples.moduleH))
    println(IOC.getSource[List[String]](tagSamples.moduleH))
//    println( IOC.getSource[List[String]](tagSamples.module5TagDecoratorPolyTypeBounds) )
    
//    println(lst.head)
//    println(lst.tail.head)
    
}