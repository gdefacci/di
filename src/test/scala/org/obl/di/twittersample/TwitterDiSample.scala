package org.obl.di
package twittersample

object TwitterDiSample extends App {

  val src = IOC.getSource[ApplicationComponentsProvider]()
  val app = IOC.get[ApplicationComponentsProvider]()
  val app1 = IOC.getSource[ApplicationComponent]("bòah")

  println(src)
  println(app1)

}
