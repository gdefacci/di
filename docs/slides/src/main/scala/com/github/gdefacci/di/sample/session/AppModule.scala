package com.github.gdefacci.di.sample.session

import javax.servlet.http.HttpSession
import scala.util.Try
import com.github.gdefacci.di.runtime.Bind

object AppModule {

  def getUser(s:Session) = s.user

  val sessionReader = (httpSession:HttpSession) => {
    for {
      usr <- Try(httpSession.getAttribute("user").asInstanceOf[User])
      data <- Try(httpSession.getAttribute("appData").asInstanceOf[AppData])
    } yield Session(usr, data)
  }
  
  val bindBySessionMyService = Bind.bind[Session => MyService]
  
}
