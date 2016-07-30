package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeFactory[C <: Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpr[C] =>
  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)

  def instantiateDag[T](knd: Kind,
    typ: Type,
    mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    mappings.get(knd.id -> typ.typeSymbol).map { dg =>
      val id = knd.id
      val dags: Seq[Dag[DagNode]] = toDagNodes(mappings, kindProvider)
      val mappings1: Map[(Id, Symbol), Dag[DagNode]] = dags.map { dg => (dg.value.kind.id -> dg.value.typ.typeSymbol) -> dg }.toMap
      mappings1(id, dg.value.typ.typeSymbol)
    }.getOrElse {
      checkIsNotPrimitive(Set(knd.id), typ)
      if (!typ.typeSymbol.isAbstract) {
        val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
          context.abort(context.enclosingPosition, s"cant find primary constructor for ${typ.typeSymbol.fullName}")
        }
        dagNodeOrRefToDagNode(constructorDag(typ, constructorMethod, mappings, kindProvider, Nil), mappings, kindProvider)

      } else {
        implementsAbstractType(typ, mappings, kindProvider)
      }
    }
  }

  private def dagNodeOrRefToDagNode(rdn: Dag[DagNodeOrRef],
    mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {
    rdn match {
      case Leaf(vdn: DagNode) => Leaf[DagNode](vdn)
      case Leaf(ref: Ref) =>
        refToDagNode(ref, mappings, kindProvider)
      case Node(vdn: DagNode, inputs) =>
        Node[DagNode](vdn, inputs.map(dagNodeOrRefToDagNode(_, mappings, kindProvider)))
      case Node(ref: Ref, inputs) =>
        context.abort(ref.sourcePos, "ref dag with inputs")
    }
  }

  def checkIsNotPrimitive(ids: Set[Id], typ: Type) = {
    if (membersSelect.isPrimitive(typ)) {
      context.abort(context.enclosingPosition, s"could not find a binding for ${typ} with id ${ids.mkString(" or ")}")
    }
  }
  
  private  def firstMapping[T](ids:Set[Id], typeSymbol:Symbol, mappings:Map[(Id, Symbol), T]):Option[(Id,T)] = {
    ids.collectFirst {
      case id if mappings.contains(id -> typeSymbol) => id -> mappings(id -> typeSymbol) 
    }
  }

  private def refToDagNode(ref: Ref,
    mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    firstMapping(ref.kinds.ids, ref.typ.typeSymbol, mappings) match {
      case Some(e) => dagNodeOrRefToDagNode(e._2, mappings, kindProvider)
      case None =>
        val subClasses = mappings.filter {
          case ((id, _), sdgn) => sdgn.value.typ != ref.typ && sdgn.value.typ.resultType <:< ref.typ && ref.kinds.ids.contains(id)
        }.toSeq
        subClasses match {
          case Seq() =>
            checkIsNotPrimitive(ref.kinds.ids, ref.typ)
            val constructorMethod = membersSelect.getPrimaryConstructor(ref.typ).getOrElse {
              context.abort(ref.sourcePos, s"cant find primary constructor for ${ref.typ.typeSymbol.fullName}")
            }
            val dnd = constructorDag(ref.typ, constructorMethod, mappings, kindProvider, Nil)
            val nmappings = dnd.value match {
              case dn:DagNode => Map((dn.kind.id -> ref.typ.typeSymbol) -> dnd)
              case _ => Map.empty
            }
            dagNodeOrRefToDagNode(dnd, mappings ++ nmappings, kindProvider)

          case Seq(hd) =>
            dagNodeOrRefToDagNode(hd._2, mappings, kindProvider)

          case ambRefs =>
            context.abort(ref.sourcePos, s"more than 1 instance available for ${ref.typ} with id ${ref.kinds.ids.mkString(" or ")} ${ambRefs.map(_._1._2).mkString(", ")}")
        }
    }
  }

  private def toDagNodes(mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): Seq[Dag[DagNode]] = {
    val z: (Map[(Id, Symbol), Dag[DagNodeOrRef]], Seq[Dag[DagNode]]) = mappings -> Nil
    mappings.values.foldLeft(z) { (acc, dagOrRef) =>
      val (mappings, resSeq) = acc
      val dagNd: Dag[DagNode] = dagNodeOrRefToDagNode(dagOrRef, mappings, kindProvider)
      val knd = dagNd.value.kind
      val typ = dagNd.value.typ
      val nmappings = (knd.id -> typ.typeSymbol) -> dagNd
      (mappings + nmappings) -> (resSeq :+ dagNd)
    }._2
  }

  private def implementsAbstractType(typ: Type,
    mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val primaryConstructor = membersSelect.getPrimaryConstructor(typ)

    val membersDagInfo: Seq[MemberDagInfo] = membersSelect.abstractMembers(typ).map(mthd => implementMethod(mthd, mappings, kindProvider))

    val members = membersDagInfo.map(_.toTree)

    primaryConstructor.fold(
      valueDag[DagNode](Nil, reflectUtils.newTrait(typ.typeSymbol, members), Kind.default))(constr =>
        dagNodeOrRefToDagNode(constructorDag(typ, constr, mappings, kindProvider, members), mappings, kindProvider))
  }

  private case class MemberDagInfo(methodName: TermName, paramss: List[List[Symbol]], returnType: Symbol, dag: Dag[DagNode]) {
    def toTree = {
      val args = paramss.map { pars =>
        pars.map { par =>
          q"""${par.asTerm.name}: ${par.info}"""
        }
      }
      val body = dagToExpr(dag).toTree
      q"""def ${methodName}(...$args):${returnType} = {
        $body
      }"""
    }
  }

  private def implementMethod(m: MethodSymbol,
    initMappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
    kindProvider: Symbol => Kinds): MemberDagInfo = {
    val parametersBindings = m.paramLists.flatMap { pars =>
      pars.flatMap { par =>
        val knd = kindProvider(par)
        knd.ids.map(id => (id -> par.info.typeSymbol) -> parameterDag(par, Kind(id, knd.scope)))
      }
    }.toMap
    val mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]] = initMappings ++ parametersBindings
//    val knd = kindProvider(m)
    MemberDagInfo(
      m.name,
      paramss = m.paramLists,
      m.returnType.typeSymbol,
      instantiateDag(Kind.default, m.returnType, mappings, kindProvider))
  }

}