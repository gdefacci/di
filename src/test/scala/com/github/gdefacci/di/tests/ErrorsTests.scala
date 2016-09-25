package com.github.gdefacci.di.tests

import org.scalatest.FunSuite
import org.scalatest.Matchers

class ErrorsTests extends  FunSuite with Matchers {
  
  test("multi bind with a incomplete instance") {
    import MultiModule._

    """IOC.get[Seq[Int]](MultiIncomplete)""" shouldNot compile

  }
  
}