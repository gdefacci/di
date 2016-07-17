package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagToExpr[C <: Context] { self:DagNodes[C] =>
  
  val context:C
  
  import context.universe._
  
  case class DagExprs(initialization: Seq[Tree], value: Tree) {

    lazy val toTree:Tree = q"""
    ..${initialization}
    ${value}
    """
  }
  
  def dagToExpr[T](dag: Dag[DagNode]): DagExprs = {
    val nds = dag.visit.collect {
      case vn:ValueNode => vn.initialization
      case mn:MethodNode => mn.initialization
    }.flatten
    val ndv = dag match {
      case Leaf(vn:ValueNode) =>
        vn.value
      case Leaf(mn:MethodNode) =>
        mn.invoke(Nil)
      case Node(n, inputs) =>
        val vs: Seq[DagExprs] = inputs.map(dn => dagToExpr(dn))
        val args = vs.map(_.value)
        n match {
          case vn:ValueNode => vn.value
          case mn:MethodNode => mn.invoke(args)
        }
    }
    DagExprs(nds, ndv)
  }
  
}