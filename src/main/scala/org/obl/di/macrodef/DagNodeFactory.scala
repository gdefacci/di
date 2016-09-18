package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodeFactory[C <: Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpr[C] with TypeResolverMixin[C] =>
  import context.universe._

  def instantiateDag[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val typeResolver = new TypeResolver(mappings, kindProvider, MapOfBuffers.empty)

    val mpng = mappings.findMembers(id, (nd) => nd.typ <:< typ)
    mpng match {
      case Seq() =>
        checkIsNotPrimitive(id, typ)
        if (!typ.typeSymbol.isAbstract) {
          typeResolver.resolveDagNodeOrRef(Ref(Kind(id, DefaultScope), typ, typ.typeSymbol.pos), Nil)
        } else {
          implementsAbstractType(typ, typeResolver, mappings, kindProvider)
        }
      case Seq(dg) =>
        typeResolver.resolveDagNodeOrRef(dg.value, dg.inputs)
      case _ => 
        context.abort(context.enclosingPosition, s"more than one instance for $id $typ ${mpng.mkString(", ")}")
    }
  }
  
  private def checkIsNotPrimitive(id: Id, typ: Type) = {
    if (membersSelect.isPrimitive(typ)) {
      context.abort(context.enclosingPosition, s"could not find a binding for $typ with id $id")
    }
  }

  private def implementsAbstractType(typ: Type,
    typeResolver: TypeResolver,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Dag[DagNode] = {

    val primaryConstructor = membersSelect.getPrimaryConstructor(typ)

    val membersDagInfo: Seq[MemberDagInfo] = membersSelect.abstractMembers(typ).map(mthd => implementMethod(mthd, mappings, kindProvider))

    val members = membersDagInfo.map(_.toTree)

    primaryConstructor.fold(
      valueDag[DagNode](Nil, reflectUtils.newTrait(typ.typeSymbol, members), Kind.default)) { constr =>
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
    val parametersBindings: Seq[(Id, Dag[DagNodeOrRef])] = m.paramLists.flatMap { pars =>
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