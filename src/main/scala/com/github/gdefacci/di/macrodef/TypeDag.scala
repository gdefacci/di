package com.github.gdefacci.di.macrodef

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import com.github.gdefacci.di.runtime.ModulesContainer

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagNodeOrRefFactory[C] with DagNodeFactory[C] with DagToExpr[C] {

  import context.universe._

  private case class ModuleMappings(members: Seq[(Id, Dag[DagNodeOrRef])], polyMembers: Seq[(Id, DagNodeDagFactory)]) {
    def addMember(id: Id, d: Dag[DagNodeOrRef]) = copy(members = members :+ (id -> d))
    def addMembers(ms: Seq[(Id, Dag[DagNodeOrRef])]) = copy(members = members ++ ms)
    def addPolyMembers(ms: Seq[(Id, DagNodeDagFactory)]) = copy(polyMembers = polyMembers ++ ms)
  }
  
  private val modulesContainerType = typeOf[ModulesContainer]

  private def createDagNodeOrRefProviders(moduleOrModuleContainerAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val exprTyp = moduleOrModuleContainerAlias.typ
    if (exprTyp <:< modulesContainerType) {
      moduleContainerDagNodeOrRefProviders(moduleOrModuleContainerAlias)
    } else {
      moduleDagNodeOrRefProviders(moduleOrModuleContainerAlias)
    }
  }
  
  private class ExprAlias(module: context.Tree, val typ:Type, val parent:Option[Dag[DagNodeOrRef]]) {
    def this(module: context.Tree, parent:Option[Dag[DagNodeOrRef]]) = this(module, module.tpe, parent)
    val termName = TermName(context.freshName(typ.typeSymbol.name.decodedName.toString))
    val dag = alias(termName, module, typ, Kind.default, parent)
  }
  
  def moduleDagNodeOrRefProviders(module: context.Expr[_]): Providers[DagNodeOrRef] = {
    createDagNodeOrRefProviders(new ExprAlias(module.tree, None))
  }
  
  def moduleDagNodeOrRefProviders(exprAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val exprNm = exprAlias.termName
    val exprDag = exprAlias.dag
    
    val membersMapping = membersSelect.getBindings(exprAlias.typ).foldLeft(ModuleMappings(Nil, Nil)) {
      case (acc, membersSelect.MethodBinding(member)) =>
        val dgs = methodDag(exprDag, exprNm, member).toSeq.flatMap {
          case dg @ Leaf(dn: DagNode) => (dn.kind.id -> dg) :: Nil
          case dg @ Node(dn: DagNode, _) => (dn.kind.id -> dg) :: Nil
          case _ => Nil
        }
        acc.addMembers(dgs)
      case (acc, membersSelect.BindInstance(member, abstractType, concreteType)) =>
        val knds = kindProvider(member)
        if (concreteType.isAbstract) {
          context.abort(member.pos, s"The second type parameter of Bind must be a concrete class, ${concreteType} is not")
        }
        acc.addMembers(knds.ids.toSeq.map(id => id -> Leaf[DagNodeOrRef](Ref(Kind(id, knds.scope), concreteType.asType.toType, member.pos))))
      case (acc, membersSelect.PolyMethodBinding(member, polyType)) =>
        val knds = kindProvider(member)
        acc.addPolyMembers(knds.ids.toSeq.map(id => id -> new PolyDagNodeFactory(Kind(id, knds.scope), Some(exprNm), member, polyType)))
    }

    val allMappings = membersMapping.addMember(exprDag.value.kind.id, exprDag)

    MProvidersMap(allMappings.members, allMappings.polyMembers)
  }
  
  def moduleContainerDagNodeOrRefProviders(moduleContainerAlias:ExprAlias): Providers[DagNodeOrRef] = {
    val mappings = MProvidersMap.empty[Id, DagNodeOrRef, DagNodeDagFactory]
    membersSelect.getValues(moduleContainerAlias.typ).map { member =>
      val memAlias = new ExprAlias(q"${moduleContainerAlias.termName}.${member.name}",member.returnType, Some(moduleContainerAlias.dag))
      mappings  ++= moduleDagNodeOrRefProviders(memAlias)
    }
    mappings
  }

  def instantiateObjectTree[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef]): Tree = {

    val dag = instantiateDag(id, typ, mappings)
    val dagExpr = dagToExpr(dag)
    context.typecheck(dagExpr.toTree)
  }

}