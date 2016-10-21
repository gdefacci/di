package com.github.gdefacci.di.tests

import javax.inject.Singleton
import com.github.gdefacci.di.runtime.Bind

object samples3 {

  case class Person(name: String, age: Int)
  case class House(owner: Person)

  object Module9 {

    val i = 33

  }

  object Module9A {

    val bindHouseFactory = Bind.bind[String => House]

  }
  
  object ByName {
    def aString(a: => Int) = a.toString
  }
  
  class Pers(val name: String, val age: Int)

  case class Cred1(i:Int, p:Pers)
  case class Cred2(b:Boolean, p:Pers)
  
  trait CredFactory {
    def f1(i:Int):Cred1
    def f2(i:Boolean):Cred2
  }
  
  object ModuleCred {
    val credFactoryBind = Bind.bind[CredFactory]
    
    @Singleton def createPers(name:String) = new Pers(name, 33)
    
    val name = "Roy"
  }
  
  trait Serv1 {
    def f(s:String):Serv3
  }
  
  class Serv2(t:String)
  
  case class Serv3A(s:Serv2)
  case class Serv3B(s:Serv2, b:Boolean)
  
  case class Serv3(a:Serv3A, b:Serv3B)
  
  object ModuleServ1 {
    
    @Singleton val bindServ2 = Bind.bind[Serv2]
    
  } 
  
}