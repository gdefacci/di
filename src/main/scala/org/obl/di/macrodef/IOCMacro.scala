package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)
    val mappings: Map[(Id, c.universe.Symbol), Dag[td.DagNodeOrRef]] = modules.flatMap { o =>
      td.toDagNodesWithRefs(o, kindProvider.apply)
    }.toMap
    td.instantiateObject(Kind.default, c.universe.weakTypeOf[T], mappings, kindProvider.apply)
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)
    val mappings: Map[(Id, c.universe.Symbol), Dag[td.DagNodeOrRef]] = modules.flatMap { o =>
      td.toDagNodesWithRefs(o, kindProvider.apply)
    }.toMap

    val dagTxt = mappings.values.map(dg => "  " + dg.value.typ.toString.padTo(30, " ").mkString + " -> " + dg).mkString("\n")
    val expr = td.instantiateObject(Kind.default, c.universe.weakTypeOf[T], mappings, kindProvider.apply)
    val resText = s"""
/*
$dagTxt
*/
${show(expr.tree)}"""
    c.Expr[String](q"${resText}")
  }
}