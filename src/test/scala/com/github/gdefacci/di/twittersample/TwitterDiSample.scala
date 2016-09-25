package com.github.gdefacci.di
package twittersample

object TwitterDiSample extends App {

  val src = IOC.getSource[ApplicationComponentsProvider]()
  val app = IOC.get[ApplicationComponentsProvider]()
  
  val grph = IOC.graph[ApplicationComponentImpl]("user")
  
  val grph1 = {
    
    import com.github.gdefacci.di.tests.samples2._

    IOC.graph[ServiceDRepo](module5BindQualifier)
  }
  
  println(grph.mkString("\n"))
  println("="*80)
  println(grph1.mkString("\n"))
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
