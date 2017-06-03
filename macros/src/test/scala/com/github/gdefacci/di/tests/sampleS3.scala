package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.Bind
import com.github.gdefacci.di.runtime.AllBindings
import com.github.gdefacci.di.runtime.ApplicationScope

object samples3 {

  case class Person(name: String, age: Int)
  case class House(owner: Person)

  object Module9 {

    val i = 33

  }

  object Module9A {

    val bindHouseFactory = Bind.bind[String => House]

  }

  class HouseFactory(val create: String => House)

  object ByName {
    def aString(a: => Int) = a.toString
  }

  class Pers(val name: String, val age: Int)

  case class Cred1(i: Int, p: Pers)
  case class Cred2(b: Boolean, p: Pers)

  trait CredFactory {
    def f1(i: Int): Cred1
    def f2(i: Boolean): Cred2
  }

  object ModuleCred {
    val credFactoryBind = Bind.bind[CredFactory]

    @ApplicationScope def createPers(name: String) = new Pers(name, 33)

    val name = "Roy"
  }

  trait Serv1 {
    def f(s: String): Serv3
  }

  class Serv2(t: String)

  case class Serv3A(s: Serv2)
  case class Serv3B(s: Serv2, b: Boolean)

  case class Serv3(a: Serv3A, b: Serv3B)

  object ModuleServ1 {

    @ApplicationScope val bindServ2 = Bind.bind[Serv2]

  }

  class ClR(i: Int, clr: => ClR)

  object ModCyc {

    def createClR(i: Int, clr: => ClR) = new ClR(i, clr)

  }

  object DecorateString {

    def decorateString(str: String): String = ">>>" + str;

  }

  object DecoratorModule1 {

    def string(i: Int) = "*" * i

    def decorateString(str: String, i: Int): String = "<" + i + ">" + str
  }

  object DecoratorModule2 {

    def decorateString(str: String, i: Int): String = "<" + i + ">" + str

    def stringSet(allstr: AllBindings[String]) = allstr.values.toSet
  }

  object DecoratorModule2aaa {

    val a = "12"
    val b = "332"
    val c = "412"
  }

  class MyFactory(val factory: Int => String)

  object MyFactoryDec {

    def decMyFactory(mf: MyFactory) =
      new MyFactory(((n: Int) => n * 2).andThen(mf.factory))
  }
  
  object InvalidPolimorphicMethod {
    
    def polyWrong[A,B](a:A, b:B):Map[String,A] = Map.empty
 
    def a = "aaa"
    def b = 12
  }
 
  object ToDel {
    
    def a = "aaa"
  }
}