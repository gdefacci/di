package com.github.gdefacci.di.tests

import com.github.gdefacci.di.IOC
import org.scalatest.FunSuite
import com.github.gdefacci.di.runtime.AllBindings

class ConstructorTests extends FunSuite {

  test("Simple object using constructor") {

    import samples1.{ Pippo, Cl0 }

    val r = IOC.get[Pippo](new Cl0(12))

    assert(r == Pippo("bah", 12))

  }

  test("Simple object using constructor with object module") {

    import samples1.{ Pippo, Cl0A }

    val r = IOC.get[Pippo](Cl0A)
    assert(r == Pippo("bah", 113))

  }

  test("Simple object using factory") {

    import samples1.{ Pippo, Cl1 }

    val r = IOC.get[Pippo](new Cl1(12))

    assert(r == Pippo("bahtrue", 100))

  }

  test("complex object 1") {

    import samples1.{ Pluto, Pippo, Cl1 }

    val r = IOC.get[Pluto](new Cl1(12))

    assert(r == Pluto(true, Pippo("bahtrue", 100), 12))

  }

  test("simple object with multi params factory") {

    import samples1.{ Pippo, Cl1A }

    val r = IOC.get[Pippo](new Cl1A)

    assert(r == Pippo("bbbb", 1333))

  }

  test("get abstract type, trait") {

    import samples2._

    val r = IOC.get[ServiceProvider](module1)
    val service: Service = r.getService(User("pippo"))

    assert(service.repository == new SqlRepo(Connection(User("pippo"))))
  }

  test("bind single instance") {

    import samples2._

    val service = IOC.get[Service](module2B, module3)

    assert(service.repository == new TestRepo(true))
  }

  test("bind 2 instances") {

    import samples2._

    val service = IOC.get[ServiceA](module4, module3)
    assert(service.repo == new TestRepo(module3.mybool))
    assert(service.httpClient == new TestHttpClient(module4.timeout))
  }

  test("provider ") {
    import samples2._

    val service = IOC.get[Service](samples2.module2, samples2.module3)
    assert(service.repository == new TestRepo(true))
  }

  test("provider 1") {
    import com.github.gdefacci.di.twittersample._

    val service = IOC.get[ApplicationComponentsProvider]()

    val usr = User("Pippo")

    assert(service.getTweeter(usr).api.user == usr)
    assert(service.getTimeline(usr).api.user == usr)

  }

  test("get abstract type, abstract class") {

    import samples2._

    val r = IOC.get[AbstractServiceProvider](module1, module3)
    assert(r.b == module3.mybool)

    val service: Service = r.getService(User("pippo"))

    assert(service.repository == new SqlRepo(Connection(User("pippo"))))
  }

  test("named") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5)
    assert(service.repo1 == new TestRepo(false))
    assert(service.repo2 == new TestRepo(true))
  }
  
  test("type tag") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5Tag)
    assert(service.repo1 == new TestRepo(false))
    assert(service.repo2 == new TestRepo(true))
  }
  
  test("type tag single source") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5TagMulti)
    assert(service.repo1 == new TestRepo(false))
    assert(service.repo2 == service.repo1)
  }

  test("qualifier") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5Qualifier)
    assert(service.repo1 == new TestRepo(false))
    assert(service.repo2 == new TestRepo(true))
  }

  test("named bind") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5Bind)
    assert(service.repo1 == new TestRepo(true))
    assert(service.repo2 == new TestRepo(false))
  }

  test("qualifier bind") {
    import samples2._

    val service = IOC.get[ServiceDRepo](samples2.module5BindQualifier)
    assert(service.repo1 == new TestRepo(true))
    assert(service.repo2 == new TestRepo(false))
  }

  test("singleton") {

    import com.github.gdefacci.di.twittersample._

    val app2 = IOC.get[ApplicationComponentImpl](TwitterModule, "user")

    assert(app2.getTweeter.api == app2.getTimeline.api)
  }

  test("singleton bind") {
    import samples2._

    val service = IOC.get[Service2](module7, module3)

    assert(service.service.repository == service.service1.repository)
  }

  test("singleton polymorphic") {
    import samples2._

    val gbc = IOC.get[GenBiGCl](module8)
    assert(gbc.a == gbc.c)
    assert(gbc.b.v == module8.b)
  }

  test("multi integers") {

    import MultiModule._
    assert(IOC.get[Seq[Int]](Mod1, Mod2, Mod3).toSet == Set(Mod1.i1, Mod1.i2, Mod2.i3, Mod3.i455))

  }

  test("multi integers modules container") {

    import MultiModule._
    assert(IOC.get[Seq[Int]](ModBag).toSet == Set(Mod1.i1, Mod1.i2, Mod2.i3, Mod3.i455))

  }

  test("named multi integers") {

    import MultiModule._
    val (seq1, seq2) = IOC.get[(Seq[Int], Seq[Int])](NMod1, NMod2, NMod3)
    assert(seq1.toSet == Set(NMod1.i2, NMod2.i2, NMod3.i455))
    assert(seq2.toSet == Set(NMod1.i1, NMod2.i1))

  }

  test("multi bind") {

    import MultiModule._
    val traits1 = IOC.get[Seq[Trait1]](NModBind)
    assert(traits1.size == 2)
    assert(traits1.exists {
      case _: Trait1ImplA => true
      case _ => false
    })
    assert(traits1.exists {
      case _: Trait1ImplB => true
      case _ => false
    })
  }

  test("class declarations in modules") {

    import samples2._

    val service = IOC.get[ServiceA](module6, module3)
    assert(service.repo == new module6.TestRepo(module3.mybool))
    assert(service.httpClient == new module6.TestHttpClient(module4.timeout))
  }

  test("polymorphic types dependencies") {
    import samples1._

    val p = IOC.get[ModGeneric.Pippo](ModGeneric)

    assert(ModGeneric.Pippo(ModGeneric.l2, ModGeneric.sumList(ModGeneric.l1)) == p)

  }

  test("polymorphic provider") {
    import samples1._

    val p1 = IOC.get[Pippo](ModPolymorphic, true)

    assert(Pippo("Pippo", 12) == p1)

    val p2 = IOC.get[Pippo](ModPolymorphic, false)

    assert(Pippo("Pippy", 27) == p2)

  }

  test("polymorphic factory") {
    import samples1._

    val p1 = IOC.get[Option[Pippo]](ModPolymorphic0)

    assert(None == p1)

  }

  test("provider with implicit param") {
    import samples1._

    val txt = IOC.get[String](ModImplicit1)

    assert(txt == "1true")
  }

  test("module with super types") {
    import samples1._

    val txt = IOC.get[String](ModImplicit1Subtyping)

    assert(txt == "1true")
  }

  test("class with implicit param") {
    import samples1._

    val txt = IOC.get[ClImplcit](ModImplicit1).text

    assert(txt == "1true1true")
  }

  test("polimorphic constructor") {
    import samples1._

    val mod = new Cl1A
    val (i, txt, b) = IOC.get[(Int, String, Boolean)](mod)

    assert(b == mod.f)
    assert(txt == mod.h)
    assert(i == 1333)

  }

  test("polimorphic constructor 1") {
    import samples1._

    val bpc = IOC.get[BibolyClient](ModBipoly, "abba")

    assert(bpc.bi.a == true)
    assert(bpc.bi.b == "abba")

  }

  test("Modules Container") {
    import samples1._

    val user = IOC.get[User](UserModule)

    assert(user.admin == UserModule3.isAdmin)
    assert(user.name == UserModule2.name)
    assert(user.id == UserModule.anInt)

  }

  test("ioc dependency 1") {
    import samples3._

    val houseFactory = IOC.get[String => House](Module9)

    assert(houseFactory("pippo") == House(Person("pippo", 33)))
  }

  test("ioc dependency 0") {
    import samples3._

    val r = IOC.get[() => Int](3)

    assert(r() == 3)
  }

  test("ioc dependency 2") {
    import samples3._

    val houseFactory = IOC.get[HouseFactory](Module9, Module9A)

    assert(houseFactory.create("pippo") == House(Person("pippo", 33)))
  }

  test("ioc dependency 3") {
    import samples3._

    val mff = IOC.get[(Int => String) => MyFactory]()

    assert("12" == mff(_.toString).factory(12))

  }

  test("by name dependency") {
    import samples3._

    val r = IOC.get[String](ByName, 3)

    assert(r == "3")
  }

  test("parameter independent singleton in trait") {
    import samples3._

    val cf = IOC.get[CredFactory](ModuleCred)

    val r1 = cf.f1(2)
    val r2 = cf.f2(true)

    assert(r1.p == r2.p)
  }

  test("parameter dependent singleton") {
    import samples3._

    val s1 = IOC.get[Serv1](ModuleServ1, true)

    val s3 = s1.f("txt")

    assert(s3.a.s == s3.b.s)
  }

  test("parameter independent singleton in function") {

    import com.github.gdefacci.di.twittersample._
    val ts = IOC.get[TweeterService1](TwitterModule1)

    val cl1 = ts.f(User("")).api.httpClient

    assert(ts.client == cl1)

  }

  test("string decorate") {
    import samples3._

    val s1 = IOC.get[String](DecorateString, "blah")

    assert(">>>blah" == s1)
  }

  test("string decorate common dependency") {
    import samples3._

    assert("<2>**" == IOC.get[String](DecoratorModule1, 2))
  }

  test("string decorate multi") {
    import samples3._

    assert(IOC.get[AllBindings[String]](DecoratorModule2, 12, "ab", "ba").values.toList == List("<12>ab", "<12>ba"))

    assert(IOC.get[Set[String]](DecoratorModule2, 12, DecoratorModule2aaa) == Set("<12>412", "<12>332", "<12>12"))
  }

  test("decorated ioc dependency 3") {
    import samples3._

    val mff = IOC.get[(Int => String) => MyFactory](MyFactoryDec)
    assert("24" == mff(_.toString).factory(12))
  }

}
