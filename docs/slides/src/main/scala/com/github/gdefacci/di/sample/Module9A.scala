package com.github.gdefacci.di.sample

trait HouseFactory {
  def apply(str:String):House
}

object Module9A {
  
    import com.github.gdefacci.di.runtime.Bind

    val bindHouseFactory = Bind.bind[HouseFactory]
  
}