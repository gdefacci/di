package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] class ReflectUtils[C <: Context](val context: C) {

  import context.universe._

  def methodCall(container: Option[TermName], method: MethodSymbol, args: Seq[Tree]) = {
    val pars = byLengthsPartition(method.paramLists.map(_.length), args.toList)
    if (method.isConstructor) {
      q"new ${method.owner}(...$pars)"
    } else {
      container match {
        case None => q"$method(...$pars)"
        case Some(container) => q"$container.$method(...$pars)"
      }
    }
  }

  def methodCall(container: Option[TermName], method: MethodSymbol, typeArgs: Seq[Type], args: Seq[Tree]) = {
    val pars = byLengthsPartition(method.paramLists.map(_.length), args.toList)
    if (method.isConstructor) {
      q"new ${method.owner}[..$typeArgs](...$pars)"
    } else {
      container match {
        case None => q"$method[..$typeArgs](...$pars)"
        case Some(container) => q"$container.$method[..$typeArgs](...$pars)"
      }
    }
  }


  def newAbstractClass(typ: Symbol, paramss: List[List[Symbol]], arguments: Seq[Tree], members: Seq[Tree]): Tree = {
    val typName = TypeName(context.freshName(typ.name.decodedName.toString))
    val pars = byLengthsPartition(paramss.map(_.length), arguments.toList)
    q"""
    class $typName extends $typ(...$pars) {
      ..$members
    }
    new $typName()
    """
  }

  def newTrait(typ: Symbol, members: Seq[Tree]): Tree =
    q"""new $typ {
      ..$members
    }"""

  def isFunctionType(t: Symbol) =
    definitions.FunctionClass.seq.contains(t)

  private def byLengthsPartition[T](lengths: Seq[Int], arguments: List[T]): List[List[T]] = {
    assert(lengths.sum <= arguments.length, s"wrong number of arguments")
    lengths match {
      case Nil => Nil
      case hd :: Nil =>
        List(arguments.take(hd))
      case hd +: rest =>
        val (curr, restArgs) = arguments.splitAt(hd)
        curr +: byLengthsPartition(rest, restArgs)
    }
  }  
  
}
