package com.github.gdefacci.di.macrodef

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.control.NonFatal

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagToExpressionFactoryMixin[C] with DagNodeOrRefFactory[C] with DagToExpr[C] {

  import context.universe._

  private def createDagNodeOrRefProviders(moduleOrModuleContainerAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val exprTyp = moduleOrModuleContainerAlias.typ
    if (membersSelect.isModuleContainerInstance(exprTyp)) {
      moduleContainerDagNodeOrRefProviders(moduleOrModuleContainerAlias)
    } else {
      moduleDagNodeOrRefProviders(moduleOrModuleContainerAlias)
    }
  }
  
  private class ExprAlias(module: context.Tree, val typ:Type, val parent:Option[Dag[DagNodeOrRef]]) {
    def this(module: context.Tree, parent:Option[Dag[DagNodeOrRef]]) = this(module, module.tpe, parent)
    val termName = TermName(context.freshName(typ.typeSymbol.name.decodedName.toString))
    val dag = alias(termName, module, typ, Kind(Global, SingletonScope), parent)
  }
  
  def moduleDagNodeOrRefProviders(module: context.Expr[_]): Providers[DagNodeOrRef] = {
    createDagNodeOrRefProviders(new ExprAlias(module.tree, None))
  }
  
  private def moduleDagNodeOrRefProviders(exprAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val exprNm = exprAlias.termName
    val exprDag = exprAlias.dag
    
    val acc = ProvidersMap.empty[Id, DagNodeOrRef, DagNodeDagFactory, Ref, Type, Decorator]
    
    membersSelect.getBindings(exprAlias.typ).foreach {
      case membersSelect.MethodBinding(member) =>
      
        val dgs = methodDag(exprDag, exprNm, member).toSeq.flatMap {
          case dg @ Dag(dn: DagNode, _) => (dn.kind.id -> dg) :: Nil
          case _ => Nil
        }
        acc.membersMap ++= dgs
        
      case membersSelect.DecoratorBinding(member, selfIndex) =>
      
        val inpDags = paramListsDags(member.paramLists).zipWithIndex flatMap {
          case (x, idx) if (idx == selfIndex) => Nil
          case (x,_) => List(x)
        } 
        
        val dec = Decorator( inpDags :+ exprDag, exprNm, member, selfIndex)
        val knds = kindProvider(member)
        
        knds.ids.toList match {
          case Nil => ()
          case Global :: Nil => ()
          case _ => context.abort(member.pos, "decorators cant be named / qualified")
        }
        if (knds.scope != DefaultScope) context.abort(member.pos, "decorators cant have scope annotation")
        
        acc.decoratorsBuffer += (member.returnType -> dec)
      
      case membersSelect.BindInstance(member, abstractType, concreteType) =>
        
        val knds = kindProvider(member)
        val refs = knds.ids.toSeq.map(id => Ref(Kind(id, knds.scope), concreteType, member.pos))
        
        acc.membersMap ++= (refs.map( r => (r.kind.id -> Dag(r) )))
        if (abstractType == concreteType) {
          acc.topLevelRefsSet ++= refs
        } 
      
      case membersSelect.ModuleContainerBinding(member, typ) =>
        
        val memAlias = new ExprAlias(q"${exprAlias.termName}.${member.name.toTermName}",typ, Some(exprAlias.dag))
        val prvdrs = moduleContainerDagNodeOrRefProviders(memAlias)
        acc ++= prvdrs
      
      case membersSelect.ObjectBinding(moduleSymbol) =>
        
        val typ = moduleSymbol.asModule.moduleClass.asType.toType  
        val memAlias = new ExprAlias(q"${exprAlias.termName}.${moduleSymbol.name}",typ, Some(exprAlias.dag))
        acc.membersMap += (memAlias.dag.value.kind.id, memAlias.dag)
        
      case membersSelect.PolyMethodBinding(member, polyType) =>
        
        val knds = kindProvider(member)
        acc.polyMembersMap ++= (knds.ids.toSeq.map(id => id -> new PolyDagNodeFactory(Kind(id, knds.scope), Some(exprNm -> exprDag), member, polyType)))
    }

    acc.membersMap += (exprDag.value.kind.id, exprDag)

    acc
  }
  
  private def moduleContainerDagNodeOrRefProviders(moduleContainerAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val mappings = ProvidersMap.empty[Id, DagNodeOrRef, DagNodeDagFactory, Ref, Type, Decorator]
    membersSelect.getValues(moduleContainerAlias.typ).map { member =>
      val typ = if (member.isModule) member.asModule.moduleClass.asType.toType 
        else if (member.isMethod) member.asMethod.returnType
        else context.abort(member.pos, "unrecognized member "+member)
      
      val memAlias = new ExprAlias(q"${moduleContainerAlias.termName}.${member.name.toTermName}",typ, Some(moduleContainerAlias.dag))
      mappings  ++= createDagNodeOrRefProviders(memAlias)
    }
    mappings
  }

  def instantiateObjectTree[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef]): Tree = {

    val ref = Ref(Kind(id, DefaultScope), typ, typ.typeSymbol.pos)
    val typeResolver = new TypeResolver(mappings, MapOfBuffers.empty, mappings.topLevelRefs + ref)

    val dag = typeResolver.resolveRef(ref)
    
    val decDag = Dag.mapValues(dag, (nd:DagNode) => nd.id) { (d, inps:Seq[Dag[DagNode]]) =>
      typeResolver.decorate(Dag(d.value, inps))
    }
    val dagTree = dagToTree(decDag)
    try {
      context.typecheck(dagTree)
    } catch {
      case NonFatal(e) => context.abort(context.enclosingPosition, e.getMessage + "\ngenerating code \n" + show(dagTree))
    }
  }

}