package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagToExpr[C <: Context] { self: DagNodes[C] =>

  val context: C

  import context.universe._

  case class DagExprs(initialization: Seq[Tree], value: Tree) {

    lazy val toTree: Tree = q"""
    ..${initialization}
    ${value}
    """
  }

  def dagToExpr[T](dag: Dag[DagNode]): DagExprs = {
    val nds: Seq[DagExprs] = Dag.visit(dag).map { n =>
      val vs: Seq[DagExprs] = n.inputs.map(dn => dagToExpr(dn))
      val inits = n.value.initialization

      val args = vs.map(_.value)
      DagExprs(inits(args), n.value.invoke(args))
    }
    DagExprs(nds.flatMap(_.initialization), nds.last.value)
  }

}