package com.github.gdefacci.di.sample.session

import javax.servlet.annotation.WebListener
import javax.servlet._
import com.github.gdefacci.di.IOC

@WebListener
class ServletApp extends ServletContextListener {
    
  def contextInitialized(sce: ServletContextEvent) = {
    val context = sce.getServletContext();

    context.addServlet("servlet", IOC.get[MyServlet](AppModule)).addMapping("/*");
  }

  def contextDestroyed(sce: ServletContextEvent) = {
  }

}