package com.github.gdefacci.di.macrodef

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.control.NonFatal

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagToExpressionFactoryMixin[C] with DagNodeOrRefFactory[C] with DagToExpr[C] with ModuleDagNodeOrRefMixin[C] {

  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)
  val moduleDagNodeOrRef = new ModuleDagNodeOrRef(membersSelect)
  
  def instantiateObjectTree[T](typ: Type,
    mappings: Providers[DagNodeOrRef]): Tree = {

    val ref = Ref(DefaultScope, typ, typ.typeSymbol.pos)
    val typeResolver = new TypeResolver(membersSelect, mappings, collection.mutable.Buffer.empty, mappings.topLevelRefs + ref)

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