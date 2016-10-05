package com.github.gdefacci.di.sample

trait Base
case class Sub1(a:Int, b:Boolean, c:String) extends Base

object Module6 {
  
  def createBase(a:Int, b:Boolean, c:String) = new Sub1(a,b,c)

}

object Module6B {
  val anInt = 5
  def aBool(i:Int) = i > 5
  val text = "5"
}
