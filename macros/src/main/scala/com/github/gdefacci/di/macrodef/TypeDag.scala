package com.github.gdefacci.di.macrodef

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.control.NonFatal

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagToExpressionFactoryMixin[C] with DagNodeOrRefFactory[C] with DagToExpr[C] {

  import context.universe._

  private case class ModuleMappings(members: Seq[(Id, Dag[DagNodeOrRef])], polyMembers: Seq[(Id, DagNodeDagFactory)], topLevelRefs:Set[Ref]) {
    def addMember(id: Id, d: Dag[DagNodeOrRef]) = copy(members = members :+ (id -> d))
    def addMembers(ms: Seq[(Id, Dag[DagNodeOrRef])]) = copy(members = members ++ ms)
    def addPolyMembers(ms: Seq[(Id, DagNodeDagFactory)]) = copy(polyMembers = polyMembers ++ ms)
    def addProviders(providers:Providers[DagNodeOrRef]) ={ 
      copy(members = members ++ providers.members.map( m => m.value.kind.id -> m ),
          polyMembers = polyMembers ++ providers.polyMembers.map( m => m.kind.id -> m),
          topLevelRefs = topLevelRefs ++ providers.topLevelRefs)
    }
    def addTopLevelRefs(refs:Seq[Ref]) = {
      copy(topLevelRefs = topLevelRefs ++ refs)
    }
  }
  
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
    
    val membersMapping = membersSelect.getBindings(exprAlias.typ).foldLeft(ModuleMappings(Nil, Nil, Set.empty)) {
      case (acc, membersSelect.MethodBinding(member)) =>
      
        val dgs = methodDag(exprDag, exprNm, member).toSeq.flatMap {
          case dg @ Dag(dn: DagNode, _) => (dn.kind.id -> dg) :: Nil
          case _ => Nil
        }
        acc.addMembers(dgs)
        
      case (acc, membersSelect.BindInstance(member, abstractType, concreteType)) =>
        
        val knds = kindProvider(member)
        val refs = knds.ids.toSeq.map(id => Ref(Kind(id, knds.scope), concreteType, member.pos))
        
        val acc1 = acc.addMembers(refs.map( r => (r.kind.id -> Dag(r) )))
        if (abstractType == concreteType) {
          acc1.addTopLevelRefs(refs)
        } else acc1
      
      case (acc, membersSelect.ModuleContainerBinding(member, typ)) =>
        
        val memAlias = new ExprAlias(q"${exprAlias.termName}.${member.name.toTermName}",typ, Some(exprAlias.dag))
        val prvdrs = moduleContainerDagNodeOrRefProviders(memAlias)
        acc.addProviders(prvdrs)
      
      case (acc, membersSelect.ObjectBinding(moduleSymbol)) =>
        
        val typ = moduleSymbol.asModule.moduleClass.asType.toType  
        val memAlias = new ExprAlias(q"${exprAlias.termName}.${moduleSymbol.name}",typ, Some(exprAlias.dag))
        acc.addMember(memAlias.dag.value.kind.id, memAlias.dag)

      case (acc, membersSelect.PolyMethodBinding(member, polyType)) =>
        
        val knds = kindProvider(member)
        acc.addPolyMembers(knds.ids.toSeq.map(id => id -> new PolyDagNodeFactory(Kind(id, knds.scope), Some(exprNm -> exprDag), member, polyType)))
        
    }

    val allMappings = membersMapping.addMember(exprDag.value.kind.id, exprDag)

    ProvidersMap(allMappings.members, allMappings.polyMembers, allMappings.topLevelRefs)
  }
  
  private def moduleContainerDagNodeOrRefProviders(moduleContainerAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val mappings = ProvidersMap.empty[Id, DagNodeOrRef, DagNodeDagFactory, Ref]
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

    val dag = typeResolver.resolveDagNodeOrRef(ref, Nil)

    val dagTree = dagToTree(dag)
    try {
      context.typecheck(dagTree)
    } catch {
      case NonFatal(e) => context.abort(context.enclosingPosition, e.getMessage + "\ngenerating code \n" + show(dagTree))
    }
  }

}