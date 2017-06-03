package com.github.gdefacci.di.tests

import com.github.gdefacci.di.runtime.{AllBindings, Bind, Tag, ^}

object tagSamples {

  object Id1
  object Id2

  import samples2._
  
  object module5TagSingleSource {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val repo1 = Tag[Id1.type with Id2.type](new TestRepo(false))
    
  }
  
  object module5Tag {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val repo1 = Tag[Id1.type](new TestRepo(false))
    val repo2 = Tag[Id2.type](new TestRepo(true))
    
  }
  
  object module5TagAndMultiMatchAll {
    
    def create(all:AllBindings[Repository ^ Any]):Seq[Repository] = all.values.map(_.value)

    val repo1 = Tag[Id1.type](new TestRepo(false))
    val repo2 = Tag[Id2.type](new TestRepo(true))
    
  }
  
  object module5TagAndMulti {
    
    def create(all:AllBindings[Repository ^ Id1.type]):Seq[Repository] = all.values.map(_.value)

    val repo1 = Tag[Id1.type](new TestRepo(false))
    val repo2 = Tag[Id1.type](new TestRepo(true))
    
    val repo3 = Tag[Id2.type](new SqlRepo(new Connection(new User("aaa"))))
    
  }

  object module5TagBindTag {
    
    def create(r1:Repository ^ Id1.type, r2:Repository ^ Id2.type) = ServiceDRepo(r1.value,r2.value)

    val bool = false
    
    val repo1 = Bind.bind[TestRepo ^ Id1.type]
    val repo2 = Bind.bind[TestRepo ^ Id2.type]
    
  }

  object module5TagDecorator {
    
    def decorateSpecific(s:String ^ Id1.type):String ^ Id1.type = {
      s.copy(value = s.value + " 1")
    }
    
    def resString(s:String ^ Id1.type):String = s.value 
    
    val input = Tag[Id1.type]("input")
    
  }
  
  object module5TagDecoratorPoly {
    
    def decorate[T](s:String ^ T):String ^ T = {
      s.copy(value = s.value + " 1")
    }
    
    def resString(s:String ^ Id1.type):String = s.value 
    
    val input = Tag[Id1.type]("input")
    
  }
  
  object module5TagDecoratorPolyK {
    
    def decorate[T](s:String ^ T):String ^ T = {
      s.copy(value = s.value + " 1")
    }
    
    def listString(s1:String ^ Id1.type, s2:String ^ Id2.type):List[String] = {
      s1.value :: s2.value :: Nil
    }
    
    val str1 = Tag[Id1.type]("aaa")
    val str2 = Tag[Id2.type]("bbb")
    
  }
  
  trait HId
  trait HId0
  case object HId1 extends HId0
  case object HId2 extends HId

  object module5TagDecoratorPolyTypeBounds {
    
    def decorate[T <: HId](s:String ^ T):String ^ T = {
      s.copy(value = s.value + " 1")
    }
    
    def decorate0[T <: HId0](s:String ^ T):String ^ T = {
      s.copy(value = s.value + " 0")
    }
    
    def listString(s1:String ^ HId1.type, s2:String ^ HId2.type):List[String] = {
      s1.value :: s2.value :: Nil
    }
    
    val str1 = Tag[HId1.type]("aaa")
    val str2 = Tag[HId2.type]("bbb")
    
  }
  
  import language.higherKinds
  
  trait Functor[F[_]] {
    def map[A,B](fa:F[A], f:A => B):F[B]
  }
  
  object moduleH {
    
    def dof[F[_]](fa:F[Int], f:Functor[F]):F[String] = f.map[Int, String](fa, a => a.toString)
    
    val opta = Some(2)
    val lista = List(2,4,8)
    
    val optFunctor:Functor[Option] = new Functor[Option] {
      def map[A,B](fa:Option[A], f:A => B):Option[B] = fa.map(f)
    }
    
    val listFunctor:Functor[List] = new Functor[List] {
      def map[A,B](fa:List[A], f:A => B):List[B] = fa.map(f)
    }
    
  }
  
}