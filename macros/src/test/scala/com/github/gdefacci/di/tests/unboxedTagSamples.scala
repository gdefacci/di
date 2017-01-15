package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.{ AllBindings, Bind }

object unboxedTagSamples {

  type Tagged[U] = { type Tag = U }
  type @@[T,U] = T with Tagged[U]
  
  class Tagger[U] {
    def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
  }
  
  def tag[U] = new Tagger[U]

  object Id1
  object Id2

  sealed trait KeyId
  object KeyId1 extends KeyId
  object KeyId2 extends KeyId
  
  import samples2._

  object module5Tag {

    def create(r1: Repository @@ Id1.type, r2: Repository @@ Id2.type) = ServiceDRepo(r1, r2)

    val repo1 = tag[Id1.type](new TestRepo(false))
    val repo2 = tag[Id2.type](new TestRepo(true))

  }

  object module5TagAndMulti {

    def create(all: AllBindings[Repository @@ Id1.type]): Seq[Repository] = all.values

    val repo1 = tag[Id1.type](new TestRepo(false))
    val repo2 = tag[Id1.type](new TestRepo(true))

    val repo3 = tag[Id2.type](new SqlRepo(new Connection(new User("aaa"))))

  }

  object module5TagBindTag {
    
    def create(r1: Repository @@ Id1.type, r2: Repository @@ Id2.type) = ServiceDRepo(r1, r2)

    val bool = false

    def bindRepo1(t:TestRepo):Repository @@ Id1.type = tag[Id1.type][TestRepo](t)
    def bindRepo2(t:TestRepo):Repository @@ Id2.type = tag[Id2.type][TestRepo](t)
    
  }

}