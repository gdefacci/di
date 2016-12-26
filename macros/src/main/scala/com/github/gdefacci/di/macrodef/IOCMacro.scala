package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Tree = {
    val td = new TypeDag[c.type](c)

    val mappings = ProvidersMap.empty[Id, td.DagNodeOrRef, td.DagNodeDagFactory, td.Ref, c.universe.Type, td.Decorator]
    modules.foreach { module =>
      mappings ++= td.moduleDagNodeOrRefProviders(module)
    }

    td.instantiateObjectTree(Global, c.universe.weakTypeOf[T], mappings)
  }

  def graph[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val dg = new DagGraph[c.type](c)

    val mappings = ProvidersMap.empty[Id, dg.td.DagNodeOrRef, dg.td.DagNodeDagFactory, dg.td.Ref, c.universe.Type, dg.td.Decorator]
    modules.foreach { module =>
      mappings ++= dg.td.moduleDagNodeOrRefProviders(module)
    }

    c.Expr(dg.graphModel(Global, c.universe.weakTypeOf[T], mappings))
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    val tree = get[T](c)(modules: _*)
    val resText = s"""${show(tree)}"""

    c.Expr[String](q"$resText")
  }

}