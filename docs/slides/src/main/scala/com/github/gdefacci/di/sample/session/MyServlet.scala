package com.github.gdefacci.di.sample.session

import scala.util._
import javax.servlet.http._

class MyServlet(
    sessionReader:HttpSession => Try[Session], 
    serviceBySession:Session => MyService) extends HttpServlet {
  
 override def doPost(req:HttpServletRequest, resp:HttpServletResponse) = {
   sessionReader(req.getSession).map(serviceBySession) match {
     case Failure(err) => resp.setStatus(500)
     case Success(service) => 
       service.doIt(10)
       resp.setStatus(200)
   }
   
 }

}