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
      case mn:MethodNode => Nil
    }.flatten
    val ndv = dag match {
      case Leaf(vn:ValueNode) =>
        vn.value
      case Leaf(mn:MethodNode) =>
        mn.invoke(Nil)
      case dn @ Node(n, inputs) =>
        val vs: Seq[DagExprs] = inputs.map(dn => dagToExpr(dn))
        val args = vs.map(_.value)
        
        n match {
          case vn:ValueNode => vn.value
          case mn:MethodNode => mn.invoke(args)
        }
    }
    DagExprs(nds, ndv)
  }
  
//  import collection.mutable.{Map => MMap}
//  
//  def dagToExpr1[T](dag: Dag[DagNode], mp:MMap[(Id, Type), DagExprs]): DagExprs = {
//    val dagValue = dag.value
//    val dagTyp = dagValue.typ
//    dagValue.kind.ids.collectFirst {
//      case id if mp.contains(id -> dagTyp) => id -> mp(id -> dagTyp)
//    } match {
//      case Some((id, dgExpr)) => 
//        assert( dagValue.kind.ids.forall(id => mp.contains(id -> dagTyp)), s"at this point map shuld contains all the ids for $dagTyp" )
//        dgExpr
//      case None =>
//        assert( dagValue.kind.ids.forall(id => !mp.contains(id -> dagTyp)), s"at this point map shuld none of the the ids for $dagTyp" )
//        
//        val z:(Seq[Tree], Seq[Tree]) = Nil -> Nil
//        dag.inputs.foldLeft(z) { (acc, inpDag) =>
//          val (inits, values) = acc
//          mp.get(inp)
//          ???
//        }
//        dag.inputs.foreach(dagToExpr1(_, mp))        
//
//        dagValue match {
//          case vn:ValueNode => vn.value
//          case mn:MethodNode => mn.invoke(args)
//        }
//    }
//    
//    dagValue.kind.ids.forall( id => mp.contains(id -> dagTyp))
//    
//    ???
//  }
  
  
}