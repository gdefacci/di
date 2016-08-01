package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)
    
    val mappings = modules.foldLeft(td.Providers.empty[td.DagNodeOrRef]) { (mappings, o) =>
      mappings ++ td.toDagNodesWithRefs(o, kindProvider.apply)
    }
    td.instantiateObject(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply)
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)
    val mappings = modules.foldLeft(td.Providers.empty[td.DagNodeOrRef]) { (mappings, module) =>
      mappings ++ td.toDagNodesWithRefs(module, kindProvider.apply)
    }
    val tree = td.instantiateObjectTree(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply)

    val dagTxt = mappings.values.map(dg => "  " + dg.value.typ.toString.padTo(30, " ").mkString + " -> " + dg).mkString("\n")
    val resText = s"""
/*
$dagTxt
*/
${show(tree)}"""
    c.Expr[String](q"${resText}")
  }
}