package com.github.gdefacci.di.sample


class MyServiceA(f:String => House) {
  def makeHose = f("myself")
}

object Module9C {

  import com.github.gdefacci.di.runtime.Bind

  val bindHouseFactory = Bind.bind[String => House]

}