package org.obl.di.tests

object samples1 {

  case class Pippo(name: String, i: Int)
  case class Pluto(b:Boolean, pippo: Pippo, i: Int)


  case class Cl0(a: Int) {

    val h = "bah"

  }

  object Cl0A {

    val a: Int = 113

    val h = "bah"

  }

  class Cl1(val a: Int) {

    val h = "bah"
    lazy val f = true

    def toPippo(nm:String, b:Boolean) = new Pippo(nm+b, 100)
    val f1: (String) => Int = str => 2

  }

  class Cl1A {
    val h = "bbbb"
    lazy val f = true

    def aIntInstance()()(b:Boolean)()()(c:String):Int = 1333
  }

  object ModGeneric {
    
    val l1:List[Int] = List(1,2,4)
    val l2:List[String] = List("1","2","4")
    
    def sumList(l:List[Int]) = l.sum
    
    case class Pippo(l1:List[String], l2:Int)
    
  }
  
}
