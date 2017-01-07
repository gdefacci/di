package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeOrRefFactory[C <: Context] { self:DagNodes[C] =>

  import context.universe._

  def alias(exprNm:TermName, valueExpr: Tree, tpe:Type, scope:DagScope, parent:Option[Dag[DagNodeOrRef]]):Dag[DagNodeOrRef] = {
    val vexpr = q"""
    val $exprNm = $valueExpr
    """
    parent.map { par =>
      Dag(DagNode.value(scope, q"$exprNm", tpe, valueExpr.pos, DagToExpression(exprNm, valueExpr)), par :: Nil)
    }.getOrElse {
      Dag(DagNode.value(scope, q"$exprNm", tpe, valueExpr.pos, DagToExpression(exprNm, valueExpr)))
    }
  }
  
  def outboundParameterDag(scope:DagScope, par:Symbol):Ref = {
    val parTyp = par.info match {
      case TypeRef(pre, k, args) if k == definitions.ByNameParamClass => 
        assert(args.length==1)
        args.head
      case _ => par.info
    }
    
    Ref(scope, parTyp, par.pos)
  }

  private def paramListsRefs(paramLists:List[List[Symbol]]):List[Ref] = 
    paramLists.flatMap(pars => pars.map { par =>
      val scope = scopeProvider(par)
      if (scope != DefaultScope) {
        context.abort(par.pos, "parameters can have scope annotation")
      }
      outboundParameterDag(scope, par)
    })
  
  def paramListsDags(paramLists:List[List[Symbol]]) =
    paramListsRefs(paramLists).map( par => Dag(par))
    
  def methodDag(container: Dag[DagNodeOrRef],
                containerTermName: TermName,
                method: Symbol): Dag[DagNodeOrRef] = {
    val paramLists = method.asMethod.paramLists
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(paramLists)
    assert(paramLists.map(_.length).sum == parametersDags.length)
    val scope = scopeProvider(method)
    Dag(DagNode.methodCall(scope, Some(containerTermName), method), parametersDags.toSeq :+ container)
  }
  
  def constructorDag(scope:DagScope,
                     exprType:Type,
                     constructorMethod:MethodSymbol,
                     members:Seq[Tree]): Dag[DagNodeOrRef] = {
    val typ = constructorMethod.owner
    if (!typ.isClass) {
      context.abort(context.enclosingPosition, typ.toString())
    }
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(constructorMethod.paramLists)
    assert(parametersDags.length == constructorMethod.paramLists.map(_.length).sum)
    Dag(DagNode.constructorCall(scope, exprType, constructorMethod, members), parametersDags)
  }

  
}
