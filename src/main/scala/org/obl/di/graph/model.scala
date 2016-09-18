package org.obl.di.graph

case class DependencyId(id:Int)
case class FilePosition(filePath:String, lineNumber:Int)

sealed trait Type
case class TypeValue(name:String) extends Type
case class PolymorphicType(resultType:String, typeParams:Seq[Type]) extends Type

sealed trait ProviderSource
case class ConstructorSource(className:String) extends ProviderSource
case class MethodSource(owner:String, methodName:String) extends ProviderSource
case object ValueSource extends ProviderSource

sealed trait DependencyScope
object DependencyScope {
  object Singleton extends DependencyScope {
    override def toString = "Singleton"
  }
  object Factory extends DependencyScope {
    override def toString = "Factory"
  }
}

case class Dependency(
    dependencyId:DependencyId,
    source:ProviderSource,
    scope:DependencyScope, 
    returnType:Type,
    position:FilePosition, 
    dependencies:Seq[DependencyId])