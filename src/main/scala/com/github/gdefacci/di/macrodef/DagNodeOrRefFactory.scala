package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeOrRefFactory[C <: Context] { self:DagNodes[C] =>

  import context.universe._

  def alias[N >: DagNode <: DagNodeOrRef](exprNm:TermName, valueExpr: Tree, tpe:Type, kind:Kind, parent:Option[Dag[N]]):Dag[N] = {
    val vexpr = q"""
    val $exprNm = $valueExpr
    """
    parent.map { par =>
      Node[N](DagNode.value(kind, vexpr :: Nil, q"$exprNm", tpe, valueExpr.pos, DagToExpression(exprNm, valueExpr)), par :: Nil)
    }.getOrElse {
      Leaf[N](DagNode.value(kind, vexpr :: Nil, q"$exprNm", tpe, valueExpr.pos, DagToExpression(exprNm, valueExpr)))
    }
  }
  
  def outboundParameterRef(knd:Kind, par:Symbol):Ref = {
    val parTyp = par.info match {
      case TypeRef(pre, k, args) if k == definitions.ByNameParamClass => 
        assert(args.length==1)
        args.head
      case _ => par.info
    }
    
    Ref(knd, parTyp, par.pos)
  }

  private def paramListsDags(paramLists:List[List[Symbol]]) = 
    paramLists.flatMap(pars => pars.map { par =>
      val knd = kindProvider(par)
      if (knd.ids.size > 1) {
        context.abort(par.pos, "parameters must have at most one identifier annotation")
      }
      Leaf[DagNodeOrRef](outboundParameterRef(Kind(knd.ids.head, knd.scope), par))
    })
    
  def methodDag(container: Dag[DagNodeOrRef],
                containerTermName: TermName,
                method: Symbol): Set[Dag[DagNodeOrRef]] = {
    val paramLists = method.asMethod.paramLists
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(paramLists)
    assert(paramLists.map(_.length).sum == parametersDags.length)
    val knds = kindProvider(method)
    knds.ids.map( id => Node[DagNodeOrRef](DagNode.methodCall(Kind(id, knds.scope), Some(containerTermName), method), parametersDags.toSeq :+ container))
  }
  
  def constructorDag(knd:Kind,
                     exprType:Type,
                     constructorMethod:MethodSymbol,
                     members:Seq[Tree]): Dag[DagNodeOrRef] = {
    val typ = constructorMethod.owner
    if (!typ.isClass) {
      context.abort(context.enclosingPosition, typ.toString())
    }
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(constructorMethod.paramLists)
    assert(parametersDags.length == constructorMethod.paramLists.map(_.length).sum)
    Node[DagNodeOrRef](DagNode.constructorCall(knd, exprType, constructorMethod, members), parametersDags)
  }

  
}
