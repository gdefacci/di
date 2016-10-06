package com.github.gdefacci.di.tests

object samples3 {

  case class Person(name: String, age: Int)
  case class House(owner: Person)

  object Module9 {

    val i = 33

  }

  object Module9A {

    import com.github.gdefacci.di.runtime.Bind

    val bindHouseFactory = Bind.bind[String => House]

  }
  
  object ByName {
    def aString(a: => Int) = a.toString
  }
}