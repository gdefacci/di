package com.github.gdefacci.di.sample

class MyService(f:HouseFactory) {
  def makeHose = f("myself")
}
