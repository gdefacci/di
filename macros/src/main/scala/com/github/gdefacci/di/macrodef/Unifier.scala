package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait Unifier[C <: blackbox.Context] { self: DagNodes[C] =>
  val context: C

  import context.universe._

  trait DagNodeDagFactory {

    def scope: DagScope
    def apply(typ: Type): Option[Dag[DagNodeOrRef]]

  }

  case class PolyDecorator(inputs: Seq[Dag[DagNodeOrRef]], containerTermName: TermName, polyDagNodeFactory: PolyDagNodeFactory, selfIndex: Int)

//  private def isSubType(t1: Type, t2: Type) = {
//    t1.etaExpand <:< t2.etaExpand
//  }
  
  private def isSameType(t1: Type, t2: Type) = {
    t1.etaExpand =:= t2.etaExpand
  }
  
  def unify(concreteType: Type, typ: Type): Option[(List[TypeSymbol], List[Type])] = {
    (concreteType, typ) match {
      case (TypeRef(prfx1, concreteBase, concrTypeArgs), TypeRef(prfx2, underlying, underlyingTypeArgs)) =>

        if (concrTypeArgs.length != underlyingTypeArgs.length) None // context.abort(context.enclosingPosition, "concrTypeArgs.length != underlyingTypeArgs.length")
        else {
           
          val z: Option[(List[TypeSymbol], List[Type])] = if (isSameType(concreteBase.asType.toType, underlying.asType.toType)) {
            Some(Nil, Nil)
          } else if (prfx2 == NoPrefix) {
            Some(List(underlying.asType) -> List(concreteBase.asType.toType.typeConstructor ))
          } else {
            None
          }
          
          if (z.isEmpty) None
          else concrTypeArgs.zip(underlyingTypeArgs).foldLeft(z) {
            case (Some((ls, ts)), (concr, gen)) =>
              gen match {
                case TypeRef(NoPrefix, typVar, Nil) =>
                  val genSym = gen.typeSymbol.asType
                  genSym.typeSignature match {
                    case TypeBounds(lo, hi) => 
                      if ((lo <:< concr) && (concr <:< hi)) {
                        Some((ls :+ genSym) -> (ts :+ concr))  
                      } else
                        None
                    case _ => 
                      Some((ls :+ genSym) -> (ts :+ concr))
                  }
                case _ =>
                  unify(concr, gen).map {
                    case (tsl, tl) => (ls ++ tsl) -> (ts ++ tl)
                  }
              }
            case (None, _) => None
          }
        }
      
      case _ =>
        None
    }
  }

  class PolyDagNodeFactory(val scope: DagScope, containerInfos: Option[(TermName, Dag[DagNodeOrRef])], val method: MethodSymbol, polyType: PolyType) extends DagNodeDagFactory {

    val (underlyingTypeArgs, underlying) = method.returnType match {
      case TypeRef(_, underlying, args) => args -> underlying
      case _ => context.abort(context.enclosingPosition, s"not a poly type method $method, return type ${method.returnType.getClass}")
    }

    override def toString = "PolyDagNodeFactory(" + method.toString + ")"

    def apply(concreteType: Type): Option[Dag[DagNodeOrRef]] = {
      if (concreteType.typeParams.nonEmpty) context.abort(context.enclosingPosition, s"type $concreteType is not a concrete type")

      val optSubstitutions = unify(concreteType, method.returnType)
      optSubstitutions.map { typesBinds =>

        val (tpKeys, tpVals) = typesBinds

        val substTypes = polyType.typeParams.map { tp =>
          val ispoly = tp.asType.toType.etaExpand match {
            case PolyType(param, result) => true
            case _ => false
          }
          if (!ispoly) tp.asType.toType.substituteTypes(tpKeys, tpVals)
          else tp.asType.toType.etaExpand.substituteTypes(tpKeys, tpVals)
        }

        val dagInputs = method.paramLists.flatMap(pars => pars.map { par =>
          val scope = scopeProvider(par)
          if (scope != DefaultScope) {
            context.abort(par.pos, "parameters cant have scope annotations")
          }
          val parTyp = par.info.substituteTypes(tpKeys, tpVals)
          Dag[DagNodeOrRef](Ref(scope, parTyp, par.pos))
        })

        val mRetType = polyType.resultType.substituteTypes(tpKeys, tpVals)
        
        val providerSource = new ProviderSource.MethodSource(method)
        val description = s"$method[${substTypes.mkString(", ")}]"
        val invoker: Seq[Tree] => Tree = { args => reflectUtils.methodCall(containerInfos.map(_._1), method, substTypes, args) }

        val nd =
          DagNode(providerSource, scope, method.name.toString, description,
            concreteType,
            method.pos,
            DagToExpression(invoker))

        Dag[DagNodeOrRef](nd, dagInputs ++ containerInfos.map(_._2).toList)
      }

    }

  }

}