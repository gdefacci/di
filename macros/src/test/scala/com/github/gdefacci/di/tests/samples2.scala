package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.Bind
import com.github.gdefacci.di.runtime.AllBindings
import com.github.gdefacci.di.runtime.ApplicationScope

object samples2 {

  trait Repository {
    def conn: Connection
  }

  case class User(name: String)
  case class Connection(user: User)

  case class SqlRepo(val conn: Connection) extends Repository

  case class TestRepo(admin: Boolean) extends Repository {
    val conn = Connection(User(if (admin) "admin" else "guest"))
  }

  trait HttpClient
  case class HttpClientImpl(user: User) extends HttpClient
  case class TestHttpClient(timeout: Int) extends HttpClient

  case class Service(val repository: Repository)

  trait ServiceProvider {
    def getService(usr: User): Service
  }

  abstract class AbstractServiceProvider(val b: Boolean) {
    def getService(usr: User): Service
  }

  case class ServiceA(httpClient: HttpClient, repo: Repository)

  object module1 {
    def repo1(conn: Connection) = new SqlRepo(conn)
  }

  object module2A {
    val repo1 = new TestRepo(true)
  }

  object module2B {
    val repo1Binding = Bind[Repository, TestRepo]
  }

  object module2 {

    def repo1(b: Boolean) = new TestRepo(b)

  }

  object module3 {
    val mybool = true
  }

  object module4 {

    val bindRepo = Bind[Repository, TestRepo]
    val bindHttp = Bind[HttpClient, TestHttpClient]

    val timeout = 10

  }
  
  case class ServiceDRepo(repo1:Repository, repo2:Repository)
  
    
  
  object module6 {

    case class TestRepo(admin: Boolean) extends Repository {
      val conn = Connection(User(if (admin) "admin" else "guest"))
    }

    case class TestHttpClient(timeout: Int) extends HttpClient

    val timeout = 10

  }

  case class Service1(val repository: Repository)
  case class Service2(service:Service, service1:Service1)

  class MyRepo extends Repository {
    def conn: Connection = Connection(User("my"))
  }
  
  object module7 {
    
    @ApplicationScope
    val bindRepo = Bind[Repository, MyRepo]
    
  }
  
  class GItm()
  class GCl[T](val v:T)
  case class GenBiGCl(a:GCl[GItm], b:GCl[Boolean], c:GCl[GItm])
  
  object module8 {
    
    val b = true
    val gitm = new GItm
    
    @ApplicationScope
    def createGCl[T](v:T):GCl[T] = new GCl[T](v)
    
  }
  
}
