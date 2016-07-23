package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] class ReflectUtils[C <: Context](val context: C) {

  import context.universe._

  private def applyParameters(exprValue: context.universe.Tree, pars: List[List[context.universe.Tree]]): context.universe.Tree = {
    pars match {
      case Nil => exprValue
      case hd :: rest => applyParameters(Apply(exprValue, hd), rest)
    }
  }

  def methodCall(container: Option[TermName], method: MethodSymbol, args: Seq[context.universe.Tree]) = {
    val (exprValue, args1) = if (method.isConstructor) {
      Select(New(Ident(method.owner)), termNames.CONSTRUCTOR) -> args.toList
    } else {
      (container match {
        case None => q"$method"
        case Some(container) => q"$container.$method"
      }) -> (args match {
        case Nil => Nil
          case _ +: tail => tail.toList
      })
    }
    val pars = Utils.byLengthsPartition(method.paramLists.map(_.length), args1)
    applyParameters(exprValue, pars)
  }

  def newAbstractClass(typ:Symbol, paramss: List[List[Symbol]], arguments:Seq[Tree], members:Seq[Tree]):Tree = {
    val typName = TypeName(context.freshName(typ.name.decodedName.toString))
    val pars = Utils.byLengthsPartition(paramss.map(_.length), arguments.toList)
    q"""
    class $typName extends $typ(...$pars) {
      ..$members
    }
    new $typName()
    """
  }

  def newTrait(typ:Symbol, members:Seq[Tree]):Tree =
    q"""new ${typ} {
      ..$members
    }"""

}
