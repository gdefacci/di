package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.ModulesContainer

object samples1 {

  case class Pippo(name: String, i: Int)
  case class Pluto(b: Boolean, pippo: Pippo, i: Int)

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

    def toPippo(nm: String, b: Boolean) = new Pippo(nm + b, 100)
    val f1: (String) => Int = str => 2

  }

  class Cl1A {
    val h = "bbbb"
    lazy val f = true

    def aIntInstance()()(b: Boolean)()()(c: String): Int = 1333
  }

  object ModGeneric {

    val l1: List[Int] = List(1, 2, 4)
    val l2: List[String] = List("1", "2", "4")

    def sumList(l: List[Int]) = l.sum

    case class Pippo(l1: List[String], l2: Int)

  }

  object ModPolymorphic {

    def toOption[T](t: T): Option[T] = Some(t)

    def createPippo(b: Option[Boolean]): Pippo = b match {
      case None => Pippo("None", 0)
      case Some(false) => Pippo("Pippy", 27)
      case Some(true) => Pippo("Pippo", 12)
    }

  }

  object ModPolymorphic0 {

    def toOption[T]: Option[T] = None

  }

  object ModImplicit1 {

    val a: Int = 1
    val s: Boolean = true

    def toText(a: Int)(implicit b: Boolean): String = a + b.toString

  }

  trait ModImplicit1Mixin1 {

    val a: Int = 1

  }

  trait ModImplicit1Mixin2 {

    val s: Boolean = true

  }

  trait ModImplicit1Mixin3 {

    def toText(a: Int)(implicit b: Boolean): String = a + b.toString

  }

  object ModImplicit1Subtyping extends ModImplicit1Mixin1 with ModImplicit1Mixin2 with ModImplicit1Mixin3

  class ClImplcit(a: Int)(implicit b: Boolean, txt: String) {
    def text = txt + a + b
  }

  case class Bipoly[A, B](a: A, b: B)

  case class BibolyClient(bi: Bipoly[Boolean, String])

  object ModBipoly {

    def boolBipoly(s: String) = Bipoly(true, s)

    def intBipoly(s: String) = Bipoly(10, s)

  }

  case class User(id: Int, name: String, admin: Boolean)

  object UserModule2 {
    def name = "Pippo"
  }

  object UserModule3 {
    val isAdmin = true
  }

  object UserModulesContainer1 extends ModulesContainer {
    val mod2 = UserModule2
    val mod3 = UserModule3
  }

  object UserModule {
    def anInt = 1222

    object another extends ModulesContainer {
      object another extends ModulesContainer {
        object another extends ModulesContainer {
          val otherModules = UserModulesContainer1
        }
      }
    }
  }

}
