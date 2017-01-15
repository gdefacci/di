package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Tree = {
    
    val td = new TypeDag[c.type](c)

    val mappings = td.emptyProviders
    
    modules.foreach { module =>
      mappings ++= td.moduleDagNodeOrRef(module)
    }

    td.instantiateObjectTree(c.universe.weakTypeOf[T], mappings)
  }

  def graph[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val dg = new DagGraph[c.type](c)
    val mappings = dg.typeDag.emptyProviders
    modules.foreach { module =>
      mappings ++= dg.typeDag.moduleDagNodeOrRef(module)
    }

    c.Expr(dg.graphModel(c.universe.weakTypeOf[T], mappings))
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    val tree = get[T](c)(modules: _*)
    val resText = s"""${show(tree)}"""

    c.Expr[String](q"$resText")
  }

}