package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context
import com.github.gdefacci.di.runtime.AllBindings
import com.github.gdefacci.di.runtime.ModulesContainer

private[di] object MembersSelect {
  val baseSkipMethods = Set("$init$", "synchronized", "##", "!=", "==", "ne", "eq", "asInstanceOf", "isInstanceOf")
  val caseClassInternalField = baseSkipMethods ++ Set("productArity", "productPrefix", "productElement", "canEqual", "productIterator")
}

private[di] class MembersSelect[C <: Context](val context: C) {

  import MembersSelect ._
  import context.universe._

  val isPrimitive = new IsPrimitive[context.type](context)
  
  private def isPublicMethod = (member: Symbol) =>
    member.isPublic && member.isMethod && !member.isConstructor && !isReserved(member)

  private def isReserved = (member:Symbol) => 
    (member.isSynthetic || member.isImplementationArtifact || member.isJava)

  private def isBindingMethod = (member: Symbol) =>
    isPublicMethod(member) && !isBindInstance(member) 

  private val bindType = typeOf[com.github.gdefacci.di.runtime.Bind[_,_]]
  private val allBindingsAny = typeOf[AllBindings[_]]

  private def isBindInstance = (member: Symbol) =>
    member.asMethod.returnType <:< bindType 

  private def isAbstractMethod = (member: Symbol) =>
    member.isMethod && member.isAbstract
    
  def getPolyType(m: MethodSymbol):Option[PolyType] = m.info match {
    case t @ PolyType(_, _) => Some(t)
    case _ => None
  }
    
  def getPolyType(m: Type):Option[PolyType] = m match {
    case t @ PolyType(_, _) => Some(t)
    case _ => None
  }
  
  private val modulesContainerType = typeOf[ModulesContainer]
  
  def isModuleContainerInstance(t:Type) = (t <:< modulesContainerType) 

  def getBindings[T](t: context.universe.Type):Seq[Binding]  = {
    
    if (isPrimitive(t)) Nil
    else {
      lazy val skipMethods =
        if (t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass) caseClassInternalField
        else baseSkipMethods
  
      t.members.collect {
        case m if isBindingMethod(m) && !skipMethods.contains(m.name.toTermName.decodedName.toString) =>
          val mthd = m.asMethod
          getPolyType(mthd).map( PolyMethodBinding(mthd, _) ).getOrElse {
            if (isValueMember(mthd) && isModuleContainerInstance(mthd.returnType)) ModuleContainerBinding(mthd, mthd.returnType)
            else MethodBinding(mthd)
          }
        case m if isPublicMethod(m) && m.asMethod.isGetter && isBindInstance(m) =>
          val bindTyp = m.asMethod.returnType
          bindTyp.typeArgs match {
            case List(abstractType, concreteType) => BindInstance(m.asMethod, abstractType.typeSymbol, concreteType.typeSymbol)
            case _ => context.abort(m.pos, s"Invalid bind type $bindTyp")
          }
        case m if m.isType && !m.isAbstract && getPrimaryConstructor(m.asType.toType).isDefined =>
          val mthd = getPrimaryConstructor(m.asType.toType).get
          getPolyType(mthd).map( PolyMethodBinding(mthd, _) ).getOrElse(MethodBinding(mthd))
        case x if x.isModule => 
          val typ = x.asModule.moduleClass.asType.toType
          if (isModuleContainerInstance(typ)) ModuleContainerBinding(x, typ)
          else ObjectBinding(x.asModule)
      }.toSeq
    }
  }
  
  private def isValueMember(mthd:MethodSymbol) = mthd.paramLists.forall(_.length==0)
  
  def getValues[T](t: context.universe.Type):Seq[Symbol]  = {
    if (isPrimitive(t)) Nil
    else {
      lazy val skipMethods =
        if (t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass) caseClassInternalField
        else baseSkipMethods
  
      t.members.flatMap {
        case m if isBindingMethod(m) && !skipMethods.contains(m.name.toTermName.decodedName.toString) =>
          val mthd = m.asMethod
          if (isValueMember(mthd)) {
            if (getPolyType(mthd).isEmpty) mthd :: Nil
            else Nil
          } else
            Nil
         case x if x.isModule  => x :: Nil
            
        case _ => Nil
      }.toSeq
    }
  }

  sealed trait Binding
  case class MethodBinding(method:MethodSymbol) extends Binding  
  case class ModuleContainerBinding(method:Symbol, typ:Type) extends Binding  
  case class PolyMethodBinding(method:MethodSymbol, methodType:PolyType) extends Binding  
  case class ObjectBinding(module:ModuleSymbol) extends Binding  
  case class BindInstance(method:MethodSymbol, abstractType:Symbol, concreteType:Symbol) extends Binding

  def abstractMembers(t: context.universe.Type): Seq[MethodSymbol] = {
    if (isPrimitive(t)) Nil
    else t.members.collect {
      case m if isAbstractMethod(m) => m.asMethod
    }.toSeq
  }

  def getPrimaryConstructor(typ: Type):Option[MethodSymbol] =
    typ.decls.collectFirst {
      case sym: MethodSymbol if sym.isPrimaryConstructor && sym.isPublic => sym
    }
  
  private def isMultiTarget(typ:Type) = {
    typ.erasure =:= allBindingsAny
  }
  
  def multiTargetItem(typ:Type):Option[Type] = {
    if (isMultiTarget(typ)) {
      val itmTyp = typ.dealias.typeArgs.head
      Some(itmTyp)
    } else 
      None
  }
}