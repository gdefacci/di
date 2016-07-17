package org.obl.di.tests

import org.obl.di.IOC
import org.scalatest.FunSuite

class ConstructorTests extends FunSuite{

  test("Simple object using constructor") {

    import samples1.{Pippo, Cl0}

    val r = IOC.get[Pippo](new Cl0(12))

    assert(r == Pippo("bah", 12))

  }

  test("Simple object using constructor with object module") {

    import samples1.{Pippo, Cl0A}

    val r = IOC.get[Pippo](Cl0A)
    assert(r == Pippo("bah", 113))

  }

  test("Simple object using factory") {

    import samples1.{Pippo, Cl1}

    val r = IOC.get[Pippo](new Cl1(12))

    assert(r == Pippo("bahtrue", 100))

  }

  test("complex object 1") {

    import samples1.{Pluto, Pippo, Cl1}

    val r = IOC.get[Pluto](new Cl1(12))

    assert(r == Pluto(true, Pippo("bahtrue", 100), 12))

  }

  test("simple object with multi params factory") {

    import samples1.{Pippo, Cl1A}

    val r = IOC.get[Pippo](new Cl1A)

    assert(r == Pippo("bbbb", 1333))

  }

  test("get abstract type, trait") {

    import samples2._

    val r = IOC.get[ServiceProvider](module1)
    val service: Service = r.getService(User("pippo"))

    assert( service.repository == new SqlRepo(Connection(User("pippo"))) )
  }

  test("bind single instance") {

    import samples2._

    val service = IOC.get[Service](module2B, module3)

    assert( service.repository == new TestRepo(true) )
  }
  
  test("bind 2 instances") {

    import samples2._

    val service = IOC.get[ServiceA](module4, module3)
    val serviceTxt = IOC.getSource[ServiceA](module4, module3)

    println(serviceTxt)
    assert( service.repo == new TestRepo(module3.mybool) )
    assert( service.httpClient == new TestHttpClient(module4.timeout) )
  }
  
  test("provider ") {
    import samples2._
    
    val service = IOC.get[Service](samples2.module2, samples2.module3)
    assert( service.repository == new TestRepo(true) )
  }

  test("get abstract type, abstract class") {

    import samples2._

    val rtxt = IOC.getSource[AbstractServiceProvider](module1, module3)

    println(rtxt)

    val r = IOC.get[AbstractServiceProvider](module1, module3)
    assert(r.b == module3.mybool)
    val service: Service = r.getService(User("pippo"))

    assert( service.repository == new SqlRepo(Connection(User("pippo"))) )
  }

  test("named ") {
    import samples2._
    
    val service = IOC.get[ServiceDRepo](samples2.module5)
    assert( service.repo1 == new TestRepo(false) )
    assert( service.repo2 == new TestRepo(true) )
  }
  
  test("named bind") {
    import samples2._
    
    val service = IOC.get[ServiceDRepo](samples2.module5Bind)
    assert( service.repo1 == new TestRepo(true) )
    assert( service.repo2 == new TestRepo(false) )
  }

}
