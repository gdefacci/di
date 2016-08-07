package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeFactory[C <: Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpr[C] =>
  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)
  
  def instantiateDag[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val mpng = mappings.all(id, (t,nd) => t.asType.toType <:< typ || nd.typ <:< typ)
    if (mpng .length > 1) context.abort(context.enclosingPosition, s"more than one instance for $id $typ ${mpng.mkString(", ")}")
    mpng.headOption.map { dg =>
      val dags: Seq[Dag[DagNode]] = toDagNodes(mappings, kindProvider)
      val mappings1: Map[(Id, Symbol), Dag[DagNode]] = dags.map { dg => (dg.value.kind.id -> dg.value.typ.typeSymbol) -> dg }.toMap
      mappings1(id, dg.value.typ.typeSymbol)
    }.getOrElse {
      checkIsNotPrimitive(id, typ)
      if (!typ.typeSymbol.isAbstract) {
        val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
          context.abort(context.enclosingPosition, s"cant find primary constructor for ${typ.typeSymbol.fullName}")
        }
        dagNodeOrRefToDagNode(constructorDag(Kind.derived, typ, constructorMethod, mappings, kindProvider, Nil), mappings, kindProvider)

      } else {
        implementsAbstractType(typ, mappings, kindProvider)
      }
    }
  }

  private def dagNodeOrRefToDagNode(rdn: Dag[DagNodeOrRef],
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {
    rdn match {
      case Leaf(vdn: DagNode) => 
        Leaf[DagNode](vdn) 
      case Leaf(ref: Ref) =>
        refToDagNode(ref, mappings, kindProvider)
      case Node(vdn: DagNode, inputs) =>
        Node[DagNode](vdn, inputs.map(dagNodeOrRefToDagNode(_, mappings, kindProvider))) 
      case Node(ref: Ref, inputs) =>
        context.abort(ref.sourcePos, "ref dag with inputs")
    }
  }

  private def checkIsNotPrimitive(id: Id, typ: Type) = {
    if (membersSelect.isPrimitive(typ)) {
      context.abort(context.enclosingPosition, s"could not find a binding for $typ with id $id")
    }
  }
  
  private def refToDagNode(ref: Ref,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val typ = ref.typ
    val typSymbol = typ.typeSymbol
    membersSelect.multiTargetItem(typ) match {
      case None =>
        val typMappings = mappings.all(ref.kind.id, { (t, nd) => 
          // FIXME cycles should be handled properly
          val r = nd != ref && ((t.asType.toType <:< typ) || (nd.typ <:< typ))    
          r 
        }).map { dg =>
          //context.warning(context.enclosingPosition, s"${dg} ")
          dagNodeOrRefToDagNode(dg, mappings, kindProvider)  
        }
        if (typMappings.length == 1) typMappings.head 
        else if (typMappings.length == 0) {
          checkIsNotPrimitive(ref.kind.id, ref.typ)
          val constructorMethod = membersSelect.getPrimaryConstructor(ref.typ).getOrElse {
            context.abort(ref.sourcePos, s"cant find primary constructor for ${typSymbol.fullName}")
          }
          val dnd = constructorDag(Kind.derived, ref.typ, constructorMethod, mappings, kindProvider, Nil)
          val nmappings = dnd.value match {
            case dn:DagNode => Map((dn.kind.id -> typSymbol) -> dnd)
            case _ => Map.empty
          }
          dagNodeOrRefToDagNode(dnd, mappings, kindProvider)
        } else
          context.abort(ref.sourcePos, s"more than 1 instance available for ${ref.typ} with id ${ref.kind.id} ${typMappings.map(_.value).mkString(", ")}")
    
      case Some(itemType) => 
        // FIXME cycles should be handled properly
        val insts:Seq[Dag[DagNodeOrRef]] = mappings.all(ref.kind.id, (t, nd) => 
          nd != ref && (t.asType.toType <:< itemType || nd.typ <:< itemType)
          )
        val nd = DagNode(Kind.default, s"allBindings$itemType", 
            inps => Nil, 
            inps => q"new org.obl.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )", 
            typ, ref.sourcePos, "")
        dagNodeOrRefToDagNode(Node(nd, insts), mappings, kindProvider)
    }
  }

  private def toDagNodes(mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Seq[Dag[DagNode]] = {
    val z: (Providers[DagNodeOrRef], Seq[Dag[DagNode]]) = (mappings, Nil)
    mappings.values.foldLeft(z) { (acc, dagOrRef) =>
      val (mappings, resSeq) = acc
        val dn: Dag[DagNode] = dagNodeOrRefToDagNode(dagOrRef, mappings, kindProvider)
        val nmapings = mappings + ((dn.value.kind.id -> dn.value.typ.typeSymbol) -> dn)
        val nres = resSeq :+ dn
        
        nmapings -> nres
    }._2
  }

  private def implementsAbstractType(typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val primaryConstructor = membersSelect.getPrimaryConstructor(typ)

    val membersDagInfo: Seq[MemberDagInfo] = membersSelect.abstractMembers(typ).map(mthd => implementMethod(mthd, mappings, kindProvider))

    val members = membersDagInfo.map(_.toTree)

    primaryConstructor.fold(
        valueDag[DagNode](Nil, reflectUtils.newTrait(typ.typeSymbol, members), Kind.default)
      )(constr =>
        dagNodeOrRefToDagNode(constructorDag(Kind.derived, typ, constr, mappings, kindProvider, members), mappings, kindProvider)
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

  private def implementMethod(m: MethodSymbol,
    initMappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): MemberDagInfo = {
    val parametersBindings = m.paramLists.flatMap { pars =>
      pars.flatMap { par =>
        val knd = kindProvider(par)
        knd.ids.map(id => (id -> par.info.typeSymbol) -> parameterDag(par, Kind(id, knd.scope)))
      }
    }.toMap
    val mappings: Providers[DagNodeOrRef] = initMappings ++ parametersBindings.toSeq
    MemberDagInfo(
      m.name,
      paramss = m.paramLists,
      m.returnType.typeSymbol,
      instantiateDag(Global, m.returnType, mappings, kindProvider))
  }

}