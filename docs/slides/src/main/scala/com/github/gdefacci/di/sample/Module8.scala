package com.github.gdefacci.di.sample

import javax.inject.Named

object Module8 {
  
  @Named("name") val name = "Foo"
  @Named("surname") val surname = "Bar"
  
}

object Module8A {
  
  def completeName(@Named("name") name:String, @Named("surname") surname:String) = s"$name $surname"
  
}