package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait TypeResolverMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] =>

  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)

  class TypeResolver(
      mappings: Providers[DagNodeOrRef],
      kindProvider: Symbol => Kinds,
      dagProviders: MapOfBuffers[Id, Dag[DagNode]]) {

    private val stack = collection.mutable.Queue.empty[DagNodeOrRef]

    private def error(msg: String) = {
      val stackLog: String = stack.reverse.map(hd => s"resolving ${hd.typ}").mkString("\n")
      context.abort(context.enclosingPosition, msg + "\n" + stackLog)
    }

    private def resolveRef(ref: Ref): Dag[DagNode] = {
      val Ref(Kind(id, _), typ, _) = ref

      val rs = dagProviders.find(id, nd => nd.value != ref && nd.value.typ <:< typ)
      rs match {
        case Seq() =>
          val r = membersSelect.multiTargetItem(typ) match {

            case Some(itemType) =>
              resolveMultiTargetRef(ref, itemType)

            case None =>
              resolveTargetRef(ref)

          }
          if (dagProviders.find(id, nd => nd.value.typ <:< typ).isEmpty) dagProviders += (id, r)
          r
        case Seq(hd) =>
          hd
        case items =>
          error(s"more than 1 instance available for ${typ} with id ${ref.kind.id} ${items.map(_.value).mkString(", ")}")
      }
    }

    def resolveDagNodeOrRef(nd: DagNodeOrRef, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      nd match {
        case ref @ Ref(_, _, _) =>
          assert(inputs.isEmpty, "refs must have no inputs")
          resolveRef(ref)
        case nd: DagNode =>
          stack.enqueue(nd)
          val r = resolveDagNode(nd, inputs)
          stack.dequeue
          r
      }
    }

    private def checkIsNotPrimitive(id: Id, typ: Type) = {
      if (membersSelect.isPrimitive(typ)) {
        error(s"could not find a binding for $typ with id $id")
      }
    }

    private def resolveTargetRef(ref: Ref): Dag[DagNode] = {
      val Ref(Kind(id, _), typ, pos) = ref
      val typMappings = mappings.findMembers(id, { nd => nd != ref && nd.typ <:< typ })
      typMappings match {
        case Seq() =>
          val polyMembers = mappings.findPolymorphicMembers(id, df => df.apply(typ))
          polyMembers match {
            case Seq() =>
              checkIsNotPrimitive(id, typ)
              val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
                error(s"cant find primary constructor for ${typ.typeSymbol.fullName}")
              }
              membersSelect.getPolyType(constructorMethod.returnType.etaExpand).map { polyType =>
                val dg = new PolyDagNodeFactory(ref.kind, None, constructorMethod, polyType, kindProvider).apply(ref.typ).getOrElse {
                  error(s"error creating dag for polymorpic primary constructor for ${typ.typeSymbol.fullName}")
                }
                resolveDagNodeOrRef(dg.value, dg.inputs)
              }.getOrElse {
                val dnd = constructorDag(ref.kind, typ, constructorMethod, kindProvider, Nil)
                resolveDagNodeOrRef(dnd.value, dnd.inputs)
              }
            case Seq(dag) =>
              resolveDagNodeOrRef(dag.value, dag.inputs)
            case _ =>
              error(s"found more than a polymorphic factory for ${typ.typeSymbol.fullName}")
          }

        case Seq(dag) =>
          resolveDagNodeOrRef(dag.value, dag.inputs)

      }
    }

    private def resolveMultiTargetRef(ref: Ref, itemType: Type) = {
      val Ref(Kind(id, _), typ, pos) = ref
      val insts: Seq[Dag[DagNodeOrRef]] = mappings.findMembers(id, (nd) => nd != ref && nd.typ <:< itemType)

      val nd = DagNode(new ProviderSource.AllbindingsSource(itemType.typeSymbol.asType), 
        Kind.default, 
        s"allBindings$itemType",
        inps => Nil,
        inps => q"new com.github.gdefacci.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )",
        typ, pos)

      resolveDagNode(nd, insts)
    }

    private def resolveDagNode(nd: DagNode, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = inputs match {
      case Seq() =>
        Leaf(nd)
      case inputs =>
        Node[DagNode](nd, inputs.map(dg => resolveDagNodeOrRef(dg.value, dg.inputs)))
    }

  }

}