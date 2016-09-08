package org.obl.di
package twittersample

object TwitterDiSample extends App {

  val src = IOC.getSource[ApplicationComponentsProvider]()
  val app = IOC.get[ApplicationComponentsProvider]()
//  val app1 = IOC.getSource[ApplicationComponent]("user")
////  val app2 = IOC.get[ApplicationComponentImpl](TwitterModule, "user")
//  val app2Src = IOC.getSource[ApplicationComponentImpl](TwitterModule, "user")
//  val app2 = IOC.get[ApplicationComponentImpl](TwitterModule, "user")
//
//  val app3Src = IOC.getSource[ApplicationComponentsProvider](TwitterModule)
  
//  assert(app2.getTweeter.api == app2.getTimeline.api)
  println(src)
////  println(app1)
//  println(app2Src)
//  println(app3Src)
//  println(app2.getTweeter.sendTweet("blah"))

}
