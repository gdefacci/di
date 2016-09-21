package com.github.gdefacci.di.macrodef

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagNodeOrRefFactory[C] with DagNodeFactory[C] with DagToExpr[C] {

  import context.universe._

  case class ModuleMappings(members: Seq[(Id, Dag[DagNodeOrRef])], polyMembers: Seq[(Id, DagNodeDagFactory)]) {
    def addMember(id: Id, d: Dag[DagNodeOrRef]) = copy(members = members :+ (id -> d))
    def addMembers(ms: Seq[(Id, Dag[DagNodeOrRef])]) = copy(members = members ++ ms)
    def addPolyMembers(ms: Seq[(Id, DagNodeDagFactory)]) = copy(polyMembers = polyMembers ++ ms)
  }

  def toDagNodesWithRefs(valueExpr: context.Expr[_], kindProvider: Symbol => Kinds): Providers[DagNodeOrRef] = {
    val exprTyp = valueExpr.actualType
    val exprNm = TermName(context.freshName(exprTyp.typeSymbol.name.decodedName.toString))
    val exprDag = alias(exprNm, valueExpr.tree, Kind.default)

    val membersMapping = membersSelect.getBindings(exprTyp).foldLeft(ModuleMappings(Nil, Nil)) {
      case (acc, membersSelect.MethodBinding(member)) =>
        val dgs = methodDag(exprDag, exprNm, member, kindProvider).toSeq.flatMap {
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
        acc.addPolyMembers(knds.ids.toSeq.map(id => id -> new PolyDagNodeFactory(Kind(id, knds.scope), Some(exprNm), member, polyType, kindProvider)))
    }

    val allMappings = membersMapping.addMember(exprDag.value.kind.id, exprDag)

    MProvidersMap(allMappings.members, allMappings.polyMembers)
  }

  def instantiateObjectTree[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Tree = {

    val dag = instantiateDag(id, typ, mappings, kindProvider)
    val dagExpr = dagToExpr(dag)
    context.typecheck(dagExpr.toTree)
  }

  //  def instantiateObject[T](id: Id,
  //    typ: Type,
  //    mappings: Providers[DagNodeOrRef],
  //    kindProvider: Symbol => Kinds): Expr[T] = {
  //    context.Expr[T](instantiateObjectTree(id, typ, mappings, kindProvider))
  //  }

}