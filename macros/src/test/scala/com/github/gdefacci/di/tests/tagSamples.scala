package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.{AllBindings, Bind}

object tagSamples {

  case class Tag[+V,+K](value:V)
  
  type ^ [+V,+K] = Tag[V,K]
  
  class WithTag[K] {
    def apply[V](v:V) = Tag[V,K](v)
  }
  
  def tag[K] = new WithTag[K]
  
  object Id1
  object Id2

  import samples2._
  
  object module5TagSingleSource {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val repo1 = tag[Id1.type with Id2.type](new TestRepo(false))
    
  }
  
  object module5Tag {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val repo1 = tag[Id1.type](new TestRepo(false))
    val repo2 = tag[Id2.type](new TestRepo(true))
    
  }
  
  object module5TagAndMultiMatchAll {
    
    def create(all:AllBindings[Repository ^ Any]):Seq[Repository] = all.values.map(_.value)

    val repo1 = tag[Id1.type](new TestRepo(false))
    val repo2 = tag[Id2.type](new TestRepo(true))
    
  }
  
  object module5TagAndMulti {
    
    def create(all:AllBindings[Repository ^ Id1.type]):Seq[Repository] = all.values.map(_.value)

    val repo1 = tag[Id1.type](new TestRepo(false))
    val repo2 = tag[Id1.type](new TestRepo(true))
    
    val repo3 = tag[Id2.type](new SqlRepo(new Connection(new User("aaa"))))
    
  }

  object module5TagBindTag {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val bool = false
    
    val repo1 = Bind.bind[TestRepo ^ Id1.type]
    val repo2 = Bind.bind[TestRepo ^ Id2.type]
    
  }

  
}