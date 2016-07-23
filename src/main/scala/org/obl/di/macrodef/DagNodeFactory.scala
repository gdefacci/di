package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeFactory[C <: Context] { self:DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpr[C] =>
  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)
  
  def instantiateDag[T](knd: Kind,
                        typ: Type,
                        mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                        kindProvider: Symbol => Kind):Dag[DagNode] = {

    firstMapping(knd.ids, typ.typeSymbol, mappings).map { 
      case (id,dg) =>
        val dags: Seq[Dag[DagNode]] = toDagNodes(mappings, kindProvider)
        val mappings1: Map[(Id, Symbol), Dag[DagNode]] = dags.flatMap { dg => dg.value.kind.ids.map( id => (id -> dg.value.typ.typeSymbol) -> dg ) }.toMap
        mappings1(id, dg.value.typ.typeSymbol)
    }.getOrElse {
      checkIsNotPrimitive(knd, typ)
      if (!typ.typeSymbol.isAbstract) {
        val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
          context.abort(context.enclosingPosition, s"cant find primary constructor for ${typ.typeSymbol.fullName}")
        }
        dagNodeOrRefToDagNode(constructorDag(constructorMethod, mappings, kindProvider, Nil), mappings, kindProvider)

      } else {
        implementsAbstractType(typ, mappings, kindProvider)
      }
    }
  }
  
  private def dagNodeOrRefToDagNode(rdn: Dag[DagNodeOrRef],
                            mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                            kindProvider: Symbol => Kind): Dag[DagNode] = {
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
  
  def checkIsNotPrimitive(kind:Kind, typ:Type) = {
    if (membersSelect.isPrimitive(typ)) {
    	  context.abort(context.enclosingPosition, s"could not find a binding for ${typ} with id ${kind.ids.mkString(",")}")
    	}
  }

  private def refToDagNode(ref: Ref,
                           mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                           kindProvider: Symbol => Kind): Dag[DagNode] = {

    firstMapping(ref.kind.ids, ref.typ.typeSymbol, mappings) match {
      case Some(e) => dagNodeOrRefToDagNode(e._2, mappings, kindProvider)
      case None =>
        val subClasses = mappings.filter {
          case ((id, _), sdgn) => sdgn.value.typ != ref.typ && sdgn.value.typ.resultType <:< ref.typ && ref.kind.ids.contains(id)
        }.toSeq
        subClasses match {
          case Seq() =>
            checkIsNotPrimitive(ref.kind, ref.typ)
            val constructorMethod = membersSelect.getPrimaryConstructor(ref.typ).getOrElse {
              context.abort(ref.sourcePos, s"cant find primary constructor for ${ref.typ.typeSymbol.fullName}")
            }
            val dnd = constructorDag(constructorMethod, mappings, kindProvider, Nil)
            val nmappings = dnd.value.kind.ids.map( id => (id -> ref.typ.typeSymbol) -> dnd ).toMap
            dagNodeOrRefToDagNode(dnd, mappings ++ nmappings, kindProvider)

          case Seq(hd) =>
            dagNodeOrRefToDagNode(hd._2, mappings, kindProvider)

          case ambRefs =>
            context.abort(ref.sourcePos, s"more than 1 instance available for ${ref.kind} ${ref.typ} ${ambRefs.map(_._1._2).mkString(", ")}")
        }
    }
  }

  private def toDagNodes(mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                          kindProvider: Symbol => Kind): Seq[Dag[DagNode]] = {
    val z: (Map[(Id, Symbol), Dag[DagNodeOrRef]], Seq[Dag[DagNode]]) = mappings -> Nil
    mappings.values.foldLeft(z) { (acc, dagOrRef) =>
      val (mappings, resSeq) = acc
      val dagNd: Dag[DagNode] = dagNodeOrRefToDagNode(dagOrRef, mappings, kindProvider)
      val knd = dagNd.value.kind
      val typ = dagNd.value.typ
      val nmappings = knd.ids.map( id => (id -> typ.typeSymbol) -> dagNd).toMap
      (mappings ++ nmappings) -> (resSeq :+ dagNd)
    }._2
  }
  
  private def implementsAbstractType(typ: Type,
                             mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                             kindProvider: Symbol => Kind):Dag[DagNode] = {

    val primaryConstructor = membersSelect.getPrimaryConstructor(typ)

    val membersDagInfo:Seq[MemberDagInfo] = membersSelect.abstractMembers(typ).map( mthd => implementMethod(mthd, mappings, kindProvider))

    val members = membersDagInfo.map(_.toTree)

    primaryConstructor.fold(
      valueDag[DagNode](Nil, reflectUtils.newTrait(typ.typeSymbol, members), Kind.default)
    )( constr =>
      dagNodeOrRefToDagNode(constructorDag(constr, mappings, kindProvider, members), mappings, kindProvider)
    )
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

  private def implementMethod(m:MethodSymbol,
                              initMappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                              kindProvider: Symbol => Kind):MemberDagInfo = {
    val parametersBindings = m.paramLists.flatMap { pars =>
      pars.flatMap { par =>
        val knd = kindProvider(par)
        knd.ids.map( id => (id -> par.info.typeSymbol) -> parameterDag(par, knd))
      }
    }.toMap
    val mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]] = initMappings ++ parametersBindings
    val knd = kindProvider(m)
    MemberDagInfo(
      m.name,
      paramss = m.paramLists,
      m.returnType.typeSymbol,
      instantiateDag(knd, m.returnType, mappings, kindProvider)
    )
  }

}