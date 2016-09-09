package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeFactory[C <: Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpr[C] with TypeResolverMixin[C] =>
  import context.universe._

//  val membersSelect = new MembersSelect[context.type](context)
  
  def instantiateDag[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val typeResolver = new TypeResolver(mappings, kindProvider, MapOfBuffers.empty)
    
    val mpng = mappings.findMembers(id, (nd) => nd.typ <:< typ ) 
    if (mpng .length > 1) context.abort(context.enclosingPosition, s"more than one instance for $id $typ ${mpng.mkString(", ")}")
    mpng.headOption.map { dg =>
      
//      dagNodeOrRefToDagNode(dg, mappings, kindProvider)
      typeResolver .resolveDagNodeOrRef(dg.value, dg.inputs)
//      val dags: Seq[Dag[DagNode]] = toDagNodes(mappings, kindProvider)
//      val mappings1: Map[(Id, Symbol), Dag[DagNode]] = dags.map { dg => (dg.value.kind.id -> dg.value.typ.typeSymbol) -> dg }.toMap
//      mappings1(id, dg.value.typ.typeSymbol)
      
    }.getOrElse {
      checkIsNotPrimitive(id, typ)
      if (!typ.typeSymbol.isAbstract) {
        val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
          context.abort(context.enclosingPosition, s"cant find primary constructor for ${typ.typeSymbol.fullName}")
        }
//        dagNodeOrRefToDagNode(constructorDag(Kind.derived, typ, constructorMethod, kindProvider, Nil), mappings, kindProvider)
        val dg = constructorDag(Kind.derived, typ, constructorMethod, kindProvider, Nil)
        typeResolver .resolveDagNodeOrRef(dg.value, dg.inputs)

      } else {
        implementsAbstractType(typ, typeResolver, mappings, kindProvider)
      }
    }
  }

//  private def dagNodeOrRefToDagNode(rdn: Dag[DagNodeOrRef],
//    mappings: Providers[DagNodeOrRef],
//    kindProvider: Symbol => Kinds): Dag[DagNode] = {
//    rdn match {
//      case Leaf(vdn: DagNode) => 
//        Leaf[DagNode](vdn) 
//      case Leaf(ref: Ref) =>
//        refToDagNode(ref, mappings, kindProvider)
//      case Node(vdn: DagNode, inputs) =>
//        Node[DagNode](vdn, inputs.map(dagNodeOrRefToDagNode(_, mappings, kindProvider))) 
//      case Node(ref: Ref, inputs) =>
//        context.abort(ref.sourcePos, "ref dag with inputs")
//    }
//  }
//
////  private def checkIsNotPrimitive(id: Id, typ: Type) = {
////    if (membersSelect.isPrimitive(typ)) {
////      context.abort(context.enclosingPosition, s"could not find a binding for $typ with id $id")
////    }
////  }
//  
//  private def refToDagNode(ref: Ref,
//    mappings: Providers[DagNodeOrRef],
//    kindProvider: Symbol => Kinds): Dag[DagNode] = {
//
//    val typ = ref.typ
//    
//    val typSymbol = typ.typeSymbol
//    membersSelect.multiTargetItem(typ) match {
//      case None =>
//        // FIXME cycles should be handled properly
//        val typMappings = mappings.findMembers(ref.kind.id, { nd => nd != ref && nd.typ <:< typ })
//        
//        if (typMappings.length == 1) dagNodeOrRefToDagNode(typMappings.head, mappings, kindProvider)
//        else if (typMappings.length == 0) {
//          
//          val polyMembers = mappings.findPolymorphicMembers(ref.kind.id, df => df.apply(typ))
//          
//          if (polyMembers.length > 1) context.abort(context.enclosingPosition, s"found more than a polymorphic factory for ${typSymbol.fullName}")
//          else if (polyMembers.length == 1) {
//            dagNodeOrRefToDagNode(polyMembers.head, mappings, kindProvider)
//          } else {
//          
//            checkIsNotPrimitive(ref.kind.id, typ)
//            val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
//              context.abort(context.enclosingPosition, s"cant find primary constructor for ${typSymbol.fullName}")
//            }
//            val dnd = constructorDag(ref.kind, typ, constructorMethod, kindProvider, Nil)
//            val r = dagNodeOrRefToDagNode(dnd, mappings, kindProvider)
//            
////            if (!mappings.replaceMember(r.value.kind.id, nd => nd.value == ref, r)) {
////              context.warning(context.enclosingPosition, r.value.toString)        
////            }
//            
//            r
//          }
//        } else {
//          context.abort(context.enclosingPosition, s"more than 1 instance available for ${typ} with id ${ref.kind.id} ${typMappings.map(_.value).mkString(", ")}")
//        }
//      case Some(itemType) => 
//        // FIXME cycles should be handled properly
//        val insts:Seq[Dag[DagNodeOrRef]] = mappings.findMembers(ref.kind.id, (nd) => 
//          nd != ref && (nd.typ <:< itemType)  
//        )
//        val nd = DagNode(Kind.default, s"allBindings$itemType", 
//            inps => Nil, 
//            inps => q"new org.obl.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )", 
//            typ, ref.sourcePos, "")
//        dagNodeOrRefToDagNode(Node(nd, insts), mappings, kindProvider)
//    }
//  }
//
////  private def toDagNodes(mappings: Providers[DagNodeOrRef],
////    kindProvider: Symbol => Kinds): Seq[Dag[DagNode]] = {
////    val z: (Providers[DagNodeOrRef], Seq[Dag[DagNode]]) = (mappings, Nil)
////    mappings.members.foldLeft(z) { (acc, dagOrRef) =>
////      val (mappings, resSeq) = acc
////        val dn: Dag[DagNode] = dagNodeOrRefToDagNode(dagOrRef, mappings, kindProvider)
////        val nmapings = mappings //+ (dn.value.kind.id  -> dn)
////        val nres = resSeq :+ dn
////        
////        nmapings -> nres
////    }._2
////  }

  private def implementsAbstractType(typ: Type,
      typeResolver:TypeResolver,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val primaryConstructor = membersSelect.getPrimaryConstructor(typ)

    val membersDagInfo: Seq[MemberDagInfo] = membersSelect.abstractMembers(typ).map(mthd => implementMethod(mthd, mappings, kindProvider))

    val members = membersDagInfo.map(_.toTree)

    primaryConstructor.fold(
        valueDag[DagNode](Nil, reflectUtils.newTrait(typ.typeSymbol, members), Kind.default)
      ) { constr =>
//        dagNodeOrRefToDagNode(constructorDag(Kind.derived, typ, constr, kindProvider, members), mappings, kindProvider)
        val dg = constructorDag(Kind.derived, typ, constr, kindProvider, members)
        typeResolver.resolveDagNodeOrRef(dg.value, dg.inputs)
      }
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
    val parametersBindings:Seq[(Id, Dag[DagNodeOrRef])] = m.paramLists.flatMap { pars =>
      pars.flatMap { par =>
        val knd = kindProvider(par)
        knd.ids.map(id => id -> parameterDag(par, Kind(id, knd.scope)))
      }
    }
    val mappings: Providers[DagNodeOrRef] = initMappings.copy() 
    mappings ++= parametersBindings
    MemberDagInfo(
      m.name,
      paramss = m.paramLists,
      m.returnType.typeSymbol,
      instantiateDag(Global, m.returnType, mappings, kindProvider))
  }

}