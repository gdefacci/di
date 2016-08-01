package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context
import org.obl.di.runtime.AllBindings

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

  private val bindType = typeOf[org.obl.di.runtime.Bind[_,_]]
  private val allBindingsAny = typeOf[AllBindings[_]]

  private def isBindInstance = (member: Symbol) =>
    member.asMethod.returnType <:< bindType 

  private def isAbstractMethod = (member: Symbol) =>
    member.isMethod && member.isAbstract

  def getBindings[T](t: context.universe.Type, 
      onBinding:MethodSymbol => T, 
      onBindInstance:BindInstance => T):Seq[T]  = {
    
    if (isPrimitive(t)) Nil
    else {
      lazy val skipMethods =
        if (t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass) caseClassInternalField
        else baseSkipMethods
  
      t.members.collect {
        case m if isBindingMethod(m) && !skipMethods.contains(m.name.toTermName.decodedName.toString) =>
          onBinding(m.asMethod)
        case m if isPublicMethod(m) && m.asMethod.isGetter && isBindInstance(m) =>
          val bindTyp = m.asMethod.returnType
          bindTyp.typeArgs match {
            case List(abstractType, concreteType) => onBindInstance(BindInstance(m.asMethod, abstractType.typeSymbol, concreteType.typeSymbol))
            case _ => context.abort(m.pos, s"Invalid bind type $bindTyp")
          }
      }.toSeq
    }
  }

  case class BindInstance(method:MethodSymbol, abstractType:Symbol, concreteType:Symbol)

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
  
  def isMultiTarget(typ:Type) = {
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