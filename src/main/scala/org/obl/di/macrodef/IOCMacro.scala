package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

object IOCMacro {

  def get[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[T] = {
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)

    val mappings = modules.foldLeft(ProvidersMap.empty[Id, td.DagNodeOrRef, td.DagNodeDagFactory]) { (mappings, o) =>
      mappings ++ td.toDagNodesWithRefs(o, kindProvider.apply)
    }
    td.instantiateObject(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply)
  }

  def getSource[T: c.WeakTypeTag](c: Context)(modules: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._
    val kindProvider = new DefaultKindProvider[c.type](c)

    val td = new TypeDag[c.type](c)
    val mappings = modules.foldLeft(ProvidersMap.empty[Id, td.DagNodeOrRef, td.DagNodeDagFactory]) { (mappings, module) =>
      mappings ++ td.toDagNodesWithRefs(module, kindProvider.apply)
    }
    val tree = td.instantiateObjectTree(Global, c.universe.weakTypeOf[T], mappings, kindProvider.apply)

    val maxLen = if (mappings.members.isEmpty) 0 else mappings.members.map(_.value.typ.toString.length).max
    val dagTxt = mappings.members.map(dg => "  " + dg.value.typ.toString.padTo(maxLen+2, " ").mkString + " -> " + dg).mkString("\n")
    val dagTxtPoly = mappings.polyMembers.map(dg => "  " + dg).mkString("\n")
    val resText = s"""
/*
$dagTxt
${if (mappings.members.nonEmpty) dagTxtPoly else ""}
*/
${show(tree)}"""
    c.Expr[String](q"${resText}")
  }

}