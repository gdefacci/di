package org.obl.di.macrodef

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with DagNodeOrRefFactory[C] with DagNodeFactory[C] with DagToExpr[C] {

  import context.universe._

  def toDagNodesWithRefs(valueExpr: context.Expr[_], kindProvider: Symbol => Kind): Map[(Id, Symbol), Dag[DagNodeOrRef]] = {
    val exprTyp = valueExpr.actualType
    val exprNm = TermName(context.freshName(exprTyp.typeSymbol.name.decodedName.toString))
    val exprDag = alias(exprNm, valueExpr.tree, kindProvider)
    val dags = exprDag +: membersSelect.bindings(exprTyp).map { member =>
      methodDag(exprDag, exprNm, member, kindProvider)
    }

    val bindInsts:Seq[((Id, Symbol), Dag[DagNodeOrRef])] = membersSelect.bindInstances(exprTyp).flatMap {
      case membersSelect.BindInstance(member, abstractType, concreteType) =>
        val knd = kindProvider(member)
        if (concreteType.isAbstract) {
          context.abort(member.pos, s"The second type parameter of Bind must be a concrete class, ${concreteType} is not")
        }
        knd.ids.map( id => (id -> abstractType) -> Leaf[DagNodeOrRef](Ref(knd, concreteType.info, member.pos)))
    }
    
    val mappingsSeq = dags.flatMap { dg =>
      dg.value.kind.ids.map( id  =>
        (id -> dg.value.typ.typeSymbol) -> dg
      )
    }
    
    val allMappings = mappingsSeq ++ bindInsts
    
    checkNoDuplicates(allMappings)
    
    allMappings.toMap
  }
  
  def checkNoDuplicates[N <: DagNodeOrRef](dags: Seq[((Id, Symbol), Dag[N])]): Unit = {
    val dups = dags.groupBy( e => e._1).filter(_._2.length > 1)
    if (dups.nonEmpty) {
      val text =
        s"""
duplicates bindings:
${dups.map { dupEntry => s"""
type ${dupEntry._1} has more than one binding
${dupEntry._2.map { v => v._2 }.mkString("\n")}
"""}.mkString("")}
"""
      context.abort(context.enclosingPosition, text)
    }
  }

  def instantiateObject[T](knd: Kind,
                           typ: Type,
                           mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                           kindProvider: Symbol => Kind): Expr[T] = {
    val dag = instantiateDag(knd, typ, mappings, kindProvider)
    val dagExpr = dagToExpr(dag)
    context.Expr[T](dagExpr.toTree)
  }

}