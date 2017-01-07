package com.github.gdefacci.di.graph

final case class DependencyId(id:Int)
final case class FilePosition(filePath:String, lineNumber:Int)

sealed trait TypeOwner
final case class Package(segments:Seq[String]) extends TypeOwner

sealed trait Type extends TypeOwner {
  def name:String
  def owner:TypeOwner
  def enclosingPackage:Package = owner match {
    case x:Package => x
    case x:Type => x.enclosingPackage
  }
}
final case class TypeValue(owner:TypeOwner, name:String) extends Type
final case class SingletonTypeValue(owner:TypeOwner, name:String) extends Type
final case class PolymorphicType(owner:TypeOwner, name:String, typeParams:Seq[Type]) extends Type

sealed trait ProviderSource
final case class ConstructorSource(className:String) extends ProviderSource
final case class MethodSource(owner:String, methodName:String) extends ProviderSource
final case class DecoratorSource(owner:String, methodName:String) extends ProviderSource
final case object ValueSource extends ProviderSource

sealed trait DependencyScope
object DependencyScope {
  object Singleton extends DependencyScope {
    override def toString = "Singleton"
  }
  object Factory extends DependencyScope {
    override def toString = "Factory"
  }
}

final case class Dependency(
    dependencyId:DependencyId,
    source:ProviderSource,
    scope:DependencyScope, 
    returnType:Type,
    position:FilePosition, 
    dependencies:Seq[DependencyId])