package com.github.gdefacci.di.tests

import org.scalatest.FunSuite
import org.scalatest.Matchers

import com.github.gdefacci.di.IOC

class ErrorsTests extends  FunSuite with Matchers {
  
  test("multi bind with a incomplete instance") {
    import MultiModule._

    """com.github.gdefacci.di.IOC.get[Seq[Int]](MultiIncomplete)""" shouldNot compile

  }
  
  test("ioc dependency 2") {
    import samples3._

    "com.github.gdefacci.di.IOC.get[HouseFactory](Module9)" shouldNot compile

  }
  
  test("class cycles detection") {
    import samples3._
    
    "com.github.gdefacci.di.IOC.get[ClR](1)" shouldNot compile
    "com.github.gdefacci.di.IOC.get[ClR](1, ModCyc)" shouldNot compile
  }
  
  test("return type does not contains all type parameters") {
    
    "com.github.gdefacci.di.IOC.get[Map[String,String]](samples3.InvalidPolimorphicMethod)" shouldNot compile
    
  }
}