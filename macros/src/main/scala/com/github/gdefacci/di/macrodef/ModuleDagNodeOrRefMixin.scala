package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait ModuleDagNodeOrRefMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with Unifier[C] with ProvidersMixin[C] =>

  import context.universe._

  private class ExprAlias(module: context.Tree, val typ: Type, val parent: Option[Dag[DagNodeOrRef]]) {
    def this(module: context.Tree, parent: Option[Dag[DagNodeOrRef]]) = this(module, module.tpe, parent)
    val termName = TermName(context.freshName(typ.typeSymbol.name.decodedName.toString))
    val dag = alias(termName, module, typ, ApplicationScope, parent)
  }
  
  def emptyProviders = ProvidersMap.empty[DagNodeOrRef, DagNodeDagFactory, Ref, Type, Decorator, PolyDecorator]
  
  class ModuleDagNodeOrRef(membersSelect: MembersSelect[context.type]) {

    def apply(module: context.Expr[_]): Providers[DagNodeOrRef] = {
      createDagNodeOrRefProviders(new ExprAlias(module.tree, None))
    }
    
    private def createDagNodeOrRefProviders(moduleOrModuleContainerAlias: ExprAlias): Providers[DagNodeOrRef] = {
      val exprTyp = moduleOrModuleContainerAlias.typ
      if (membersSelect.isModuleContainerInstance(exprTyp)) {
        moduleContainerDagNodeOrRefProviders(moduleOrModuleContainerAlias)
      } else {
        moduleDagNodeOrRefProviders(moduleOrModuleContainerAlias)
      }
    }
    
    def skipIndex[T](i:Int, seq:Seq[T]):Seq[T] = {
      seq.zipWithIndex flatMap {
        case (x, idx) if (idx == i) => Nil
        case (x, _) => List(x)
      }
    }

    private def moduleDagNodeOrRefProviders(exprAlias: ExprAlias): Providers[DagNodeOrRef] = {
      val exprNm = exprAlias.termName
      val exprDag = exprAlias.dag

      val acc = emptyProviders

      membersSelect.getBindings(exprAlias.typ).foreach {
        case membersSelect.MethodBinding(member) =>

          val dg = methodDag(exprDag, exprNm, member)
          acc.members += dg

        case membersSelect.DecoratorBinding(member, selfIndex) =>

          val inpDags =  skipIndex(selfIndex, paramListsDags(member.paramLists))

          val dec = Decorator(inpDags :+ exprDag, exprNm, member, selfIndex)
          val scope = scopeProvider(member)

          if (scope != DefaultScope) context.abort(member.pos, "decorators cant have scope annotation")

          acc.decoratorsBuffer += (member.returnType -> dec)

        case membersSelect.BindInstance(member, abstractType, concreteType) =>

          val scope = scopeProvider(member)
          val ref = Ref(scope, concreteType, member.pos)

          acc.members += Dag(ref) 
          if (abstractType == concreteType) {
            acc.topLevelRefsSet += ref
          }

        case membersSelect.ModuleContainerBinding(member, typ) =>

          val memAlias = new ExprAlias(q"${exprAlias.termName}.${member.name.toTermName}", typ, Some(exprAlias.dag))
          val prvdrs = moduleContainerDagNodeOrRefProviders(memAlias)
          acc ++= prvdrs

        case membersSelect.ObjectBinding(moduleSymbol) =>

          val typ = moduleSymbol.asModule.moduleClass.asType.toType
          val memAlias = new ExprAlias(q"${exprAlias.termName}.${moduleSymbol.name}", typ, Some(exprAlias.dag))
          acc.members += memAlias.dag

        case membersSelect.PolyMethodBinding(member, polyType) =>

          val knds = scopeProvider(member)
          acc.polyMembers += new PolyDagNodeFactory(knds, Some(exprNm -> exprDag), member, polyType)
          
       case membersSelect.PolyDecoratorBinding(member, polyType, selfIndex) =>

          val inpDags =  skipIndex(selfIndex, paramListsDags(member.paramLists))

          val dec = PolyDecorator(inpDags :+ exprDag, exprNm, new PolyDagNodeFactory(DefaultScope, Some(exprNm -> exprDag), member, polyType), selfIndex)
          val scope = scopeProvider(member)

          if (scope != DefaultScope) context.abort(member.pos, "decorators cant have scope annotation")

          acc.polyDecoratorsBuffer += dec
   
      }

      acc.members += exprDag

      acc
    }

    private def moduleContainerDagNodeOrRefProviders(moduleContainerAlias: ExprAlias): Providers[DagNodeOrRef] = {
      val mappings = emptyProviders
      membersSelect.getValues(moduleContainerAlias.typ).map { member =>
        val typ = if (member.isModule) member.asModule.moduleClass.asType.toType
        else if (member.isMethod) member.asMethod.returnType
        else context.abort(member.pos, "unrecognized member " + member)

        val memAlias = new ExprAlias(q"${moduleContainerAlias.termName}.${member.name.toTermName}", typ, Some(moduleContainerAlias.dag))
        mappings ++= createDagNodeOrRefProviders(memAlias)
      }
      mappings
    }

  }

}