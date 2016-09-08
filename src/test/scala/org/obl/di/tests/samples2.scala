package org.obl.di.tests

import javax.inject.Named

import org.obl.di.runtime.{Bind}
import scala.annotation.StaticAnnotation
import javax.inject.Qualifier
import scala.annotation.ClassfileAnnotation
import javax.inject.Singleton

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
  
  object module5 {
    
    def create(@Named("repo1") r1:Repository, r2:Repository) = ServiceDRepo(r1,r2)

    @Named("repo1") val repo1 = new TestRepo(false)
    val repo2 = new TestRepo(true)
    
  }
  
  object module5Bind {
    
    def create(@Named("repo1") r1:Repository, r2:Repository) = ServiceDRepo(r1,r2)

    @Named("repo1") val bindRepo = Bind[Repository, TestRepo]
    
    val mybool = true
    
    def repo2(@Named("repo2") b2:Boolean) = new TestRepo(b2)
    
    @Named("repo2") val myboolRepo2 = false
  }

  object module5Qualifier {

    def create(@Qual1(value = 1, name = "aaa") r1:Repository, r2:Repository) = ServiceDRepo(r1,r2)

    @Qual1(value = 1, name = "aaa") val repo1 = new TestRepo(false)
    val repo2 = new TestRepo(true)

  }

  object module5BindQualifier {

    def create(@Qual1(value = 1, name = "aaa") r1:Repository, r2:Repository) = ServiceDRepo(r1,r2)

    @Named("blah blah") @Named("blah blah 1") @Qual1(value = 1, name = "aaa") val bindRepo = Bind[Repository, TestRepo]

    val mybool = true

    def repo2(@Qual1(value = 2, name = "aaa") b2:Boolean) = new TestRepo(b2)

    @Qual1(value = 2, name = "aaa") val myboolRepo2 = false
  }
  
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
    
    @Singleton
    val bindRepo = Bind[Repository, MyRepo]
    
  }
}
