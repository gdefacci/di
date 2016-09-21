package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Tree = {
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)

    val mappings = MProvidersMap.empty[Id, td.DagNodeOrRef, td.DagNodeDagFactory]
    modules.foreach { module =>
      mappings ++= td.toDagNodesWithRefs(module, kindProvider.apply)
    }

    td.instantiateObjectTree(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply)
  }

  def graph[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val kindProvider = new DefaultKindProvider[c.type](c)

    val dg = new DagGraph[c.type](c)

    val mappings = MProvidersMap.empty[Id, dg.td.DagNodeOrRef, dg.td.DagNodeDagFactory]
    modules.foreach { module =>
      mappings ++= dg.td.toDagNodesWithRefs(module, kindProvider.apply)
    }

    c.Expr(dg.graphModel(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply))
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    val tree = get[T](c)(modules: _*)
    val resText = s"""${show(tree)}"""

    c.Expr[String](q"${resText}")
  }

}