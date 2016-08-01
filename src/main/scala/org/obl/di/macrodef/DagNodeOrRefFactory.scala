package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeOrRefFactory[C <: Context] { self:DagNodes[C] =>

  import context.universe._

  def alias[N >: DagNode <: DagNodeOrRef](exprNm:TermName, valueExpr: Tree, kind:Kind):Dag[N] = {
    val vexpr = q"""
    val $exprNm = $valueExpr
    """
    Leaf[N](DagNode.value(kind, vexpr :: Nil, q"$exprNm", valueExpr.tpe, valueExpr.pos))
  }
  
  private def getTyp(v:Tree) = {
    if (v.tpe != null) v.tpe
    else context.typecheck(q"$v", context.TYPEmode).tpe
  }

  def valueDag[N >: DagNode <: DagNodeOrRef](initialization:Seq[Tree], v:Tree, kind:Kind):Dag[N] = {
    val typ = getTyp(q"""
    ..$initialization
    $v
    """)
    Leaf(DagNode.value(kind, initialization, v,  typ, v.pos))  }

  private def paramListsDags(paramLists:List[List[Symbol]], kindProvider: Symbol => Kinds) = 
    paramLists.flatMap(pars => pars.map { par =>
      val knd = kindProvider(par)
      if (knd.ids.size > 1) {
        context.abort(par.pos, "parameters that are not multi target must have at most one identifier annotation")
      }
      Leaf[DagNodeOrRef](Ref(Kind(knd.ids.head, knd.scope), par.info, par.pos))
    })

  def methodDag(container: Dag[DagNodeOrRef],
                containerTermName: TermName,
                method: Symbol,
                kindProvider: Symbol => Kinds): Set[Dag[DagNodeOrRef]] = {
    val paramLists = method.asMethod.paramLists
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(paramLists, kindProvider)
    assert(paramLists.map(_.length).sum == parametersDags.length)
    val knds = kindProvider(method)
    knds.ids.map( id => Node[DagNodeOrRef](DagNode.methodCall(Kind(id, knds.scope), Some(containerTermName), method), parametersDags.toSeq :+ container))
  }
  
  def constructorDag(knd:Kind,
                     exprType:Type,
                     constructorMethod:MethodSymbol,
                     mappings: Providers[DagNodeOrRef],
                     kindProvider: Symbol => Kinds,
                     members:Seq[Tree]): Dag[DagNodeOrRef] = {
    val typ = constructorMethod.owner
    if (!typ.isClass) {
      context.abort(context.enclosingPosition, typ.toString())
    }
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramListsDags(constructorMethod.paramLists, kindProvider)
    assert(parametersDags.length == constructorMethod.paramLists.map(_.length).sum)
    Node[DagNodeOrRef](DagNode.constructorCall(knd, None, exprType, constructorMethod, members), parametersDags)
  }

  def parameterDag(par:Symbol, knd:Kind): Dag[DagNodeOrRef]  =
    Leaf[DagNodeOrRef](DagNode.value(knd, Nil, q"${par.asTerm.name}", par.info, par.pos))
}
