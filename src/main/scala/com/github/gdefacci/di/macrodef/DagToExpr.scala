package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagToExpr[C <: Context] { self: DagNodes[C] =>

  val context: C

  import context.universe._
  import collection.mutable.{Set => MSet}
  
  private class DefaultDagToTree(val dagNode:SimpleDagNode, val dag: Dag[DagNode], val dependencies:Seq[DagToTree]) extends DagToTree {

    val id = dag.value.id
    private lazy val args = dependencies.map(_.value)
    
    private val isSingleton = dag.value.kind.scope == SingletonScope
    private lazy val invokeValue = dagNode.invoke(args)
    
    private lazy val singletonName = TermName(context.freshName("singleton"+dag.value.name))

    lazy val localInitialization:Seq[Tree] = {
      val inits = dagNode.initialization(args)
      if (isSingleton) {
        val init = q"val ${singletonName} = $invokeValue"
        inits :+ init
      } else
        inits
    }
    
    lazy val initialization:Seq[Tree] =  DagToTree.distinct(dependencies).flatMap { d => d.localInitialization  } ++ localInitialization
    
    lazy val value:Tree = if (isSingleton) q"${singletonName}" else invokeValue
          
  }
  
  private class AbstractTypeDagToTree(val dagNode:AbstractTypeDagNode,  val dag: Dag[DagNode], val dependencies:Seq[DagToTree])  extends DagToTree{

    val id = dag.value.id
    
    private val isSingleton = dag.value.kind.scope == SingletonScope
    private lazy val invokeValue = dagNode.invoke(dependencies)
    
    private lazy val singletonName = TermName(context.freshName("singleton"+dag.value.name))

    lazy val localInitialization:Seq[Tree] = {
      if (isSingleton) {
        val init = q"val ${singletonName} = $invokeValue"
        Seq(init)
      } else
        Nil
    }
    
    lazy val initialization:Seq[Tree] = dagNode.initialization(dependencies)
    
    lazy val value:Tree = if (isSingleton) q"${singletonName}" else invokeValue
          
  }
  
  private def toTree(dtt:DagToTree):Tree = {
     q"""..${dtt.initialization}
          ${dtt.value}"""
  }

  
  def dagToTree1[T](dag: Dag[DagNode]): Tree = {
    toTree(Dag.mapValues[DagNode, Int, DagToTree](dag, _.id)((d, inps) => 
      d.value match {
        case nd:SimpleDagNode =>  new DefaultDagToTree(nd, d, inps)
        case nd:AbstractTypeDagNode => new AbstractTypeDagToTree(nd, d, inps)
      }
    ))
  }
  
  
  val dagToTreeFun = Gen.fromDag[DagNode, Int](_.id, (d, inpts) => DagToExpression.singletonize(d.value.dagToExpression)(d, inpts)) _
  
  def dagToTree[T](dag: Dag[DagNode]): Tree = {
    val r = dagToTreeFun(dag)
    DagToExpression.expressionToTree(r.value)
  }

}