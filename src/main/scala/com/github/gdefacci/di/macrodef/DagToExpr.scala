package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagToExpr[C <: Context] { self: DagNodes[C] =>

  val context: C

  import context.universe._
  import collection.mutable.{Set => MSet}
  
  class DagToTree(val dag: Dag[DagNode], val dependencies:Seq[DagToTree]) {

    private val id = dag.value.id
    private lazy val args = dependencies.map(_.value)
    
    private val isSingleton = dag.value.kind.scope == SingletonScope
    private val v = dag.value.invoke(args)
    
    private def distinctDependenciesInitialiation(visited:MSet[Int]):Seq[Tree] = {
      dependencies.flatMap{ d =>
        if (visited.contains(d.id)) Nil
        else {
          visited += d.id
          val res1 = d.distinctDependenciesInitialiation(visited) 
          res1 ++ d.localInitialization  
        }
      } 
    }
    
    private lazy val singletonName = TermName(context.freshName("singleton"+dag.value.name))

    private lazy val localInitialization:Seq[Tree] = {
      val inits = dag.value.initialization(args)
      if (isSingleton) {
        val init = q"val ${singletonName} = $v"
        inits :+ init
      } else
        inits
    }
    
    lazy val initialization:Seq[Tree] = distinctDependenciesInitialiation(MSet(id)) ++ localInitialization
    
    lazy val value:Tree = if (isSingleton) q"${singletonName}" else v
          
    lazy val toTree: Tree = 
      q"""..${initialization}
          ${value}"""
    
  }

  
  def dagToExpr[T](dag: Dag[DagNode]): DagToTree = {
    Dag.mapValues[DagNode, Int, DagToTree](dag, _.id)((d, inps) => new DagToTree(d, inps))
  }

}