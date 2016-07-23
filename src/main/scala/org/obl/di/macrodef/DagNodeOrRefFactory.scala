package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeOrRefFactory[C <: Context] { self:DagNodes[C] =>

  import context.universe._

  def alias[N >: ValueDagNode <: DagNodeOrRef](exprNm:TermName, valueExpr: Tree, kind:Kind):Dag[N] = {
    val vexpr = q"""
    val $exprNm = $valueExpr
    """
    Leaf[N](new ValueDagNode(kind, vexpr :: Nil, q"$exprNm", valueExpr.tpe, valueExpr.pos))
  }

  def valueDag[N >: ValueDagNode <: DagNodeOrRef](initialization:Seq[Tree], v:Tree, kind:Kind):Dag[N] =
    Leaf(new ValueDagNode(kind, initialization, v,  v.tpe, v.pos))

  def methodDag(container: Dag[DagNodeOrRef],
                containerTermName: TermName,
                method: Symbol,
                kindProvider: Symbol => Kind): Dag[DagNodeOrRef] = {
    val paramLists = method.asMethod.paramLists
    val parametersDags: Seq[Dag[DagNodeOrRef]] = paramLists.flatMap(pars => pars.map(par =>
      Leaf[DagNodeOrRef](new Ref(kindProvider(par), par.info, par.pos))
    ))
    assert(paramLists.map(_.length).sum == parametersDags.length)
    Node[DagNodeOrRef](new MethodDagNode(kindProvider(method), Some(containerTermName), method, Nil), container +: parametersDags.toSeq)
  }
  
  def firstMapping[T](ids:Set[Id], typeSymbol:Symbol, mappings:Map[(Id, Symbol), T]):Option[(Id,T)] = {
    val z:Option[(Id,T)] = None
    ids.foldLeft(z) { (acc, id) =>
      acc.orElse {
        mappings.get(id -> typeSymbol).map(v => id -> v) 
      }
    }
  }
  
  def constructorDag(constructorMethod:MethodSymbol,
                     mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                     kindProvider: Symbol => Kind,
                     members:Seq[Tree]): Dag[DagNodeOrRef] = {
    val typ = constructorMethod.owner
    if (!typ.isClass) {
      context.abort(context.enclosingPosition, typ.toString())
    }
    val parametersDags: Seq[Dag[DagNodeOrRef]] = constructorMethod.paramLists.flatMap(pars =>
      pars.map { par =>
        val knd = kindProvider(par)
        firstMapping(knd.ids, par.info.typeSymbol, mappings).map(_._2).getOrElse(
            Leaf[DagNodeOrRef](new Ref(knd, par.info, par.pos))
        )
      }).toSeq
    val knd = kindProvider(constructorMethod)
    assert(parametersDags.length == constructorMethod.paramLists.map(_.length).sum)
    Node[DagNodeOrRef](new ConstructorDagNode(knd, None, constructorMethod, members, Nil), parametersDags)
  }

  def parameterDag(par:Symbol, knd:Kind) =
    Node[DagNodeOrRef](new ValueDagNode(knd, Nil, q"${par.asTerm.name}", par.info, par.pos), Nil)
}
