package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagToExpr[C <: Context] { self: DagNodes[C] =>

  val context: C

  import context.universe._
  import collection.mutable.{Set => MSet}
  
//  val dagToTreeFun = Gen.fromDag[DagNode, Int](_.id, (d, inpts) => DagToExpression.singletonize(d.value.dagToExpression)(d, inpts)) _
  val dagToTreeFun = Gen.fromDag[DagNode, Int](_.id, (d, inpts) => d.value.dagToExpression(d, inpts)) _
  
  def dagToTree[T](dag: Dag[DagNode]): Tree = {
    val r = dagToTreeFun(dag)
    genExpressionToTree(r.value)
  }

}