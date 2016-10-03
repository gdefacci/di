package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait TypeResolverMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] =>

  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)

  class TypeResolver(
      mappings: Providers[DagNodeOrRef],
      dagProviders: MapOfBuffers[Id, Dag[DagNode]],
      stack: collection.mutable.Queue[DagNodeOrRef] = collection.mutable.Queue.empty[DagNodeOrRef]) {

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
          error(s"more than one instance of $typ ${describeId(id)}: ${items.map(_.value).mkString(", ")}")
      }
    }

    private def describeId(id: Id) = id match {
      case Global | Derived => ""
      case WithName(nm) => s"with name $nm"
      case WithQualifier(nm, mp) => s"with qualifier $nm (${mp.mkString(", ")})"
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
        error(s"could not find a binding for $typ ${describeId(id)}")
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
              if (typ.erasure <:< weakTypeOf[Function1[_, _]]) {
                val Seq(in,out) = typ.typeArgs
                implementsFunction(ref.kind, typ, ref.sourcePos, in, out)
              } else if (typ.typeSymbol.isAbstract) {
                implementsAbstractType(ref.kind, typ)
              } else {
                val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
                  error(s"cant find primary constructor for ${typ.typeSymbol.fullName} resolving $typ ${describeId(id)}")
                }
                membersSelect.getPolyType(constructorMethod.returnType.etaExpand).map { polyType =>
                  val dg = new PolyDagNodeFactory(ref.kind, None, constructorMethod, polyType).apply(ref.typ).getOrElse {
                    error(s"error creating dag for polymorpic primary constructor for ${typ.typeSymbol.fullName} resolving $typ ${describeId(id)}")
                  }
                  resolveDagNodeOrRef(dg.value, dg.inputs)
                }.getOrElse {
                  val dnd = constructorDag(ref.kind, typ, constructorMethod, Nil)
                  resolveDagNodeOrRef(dnd.value, dnd.inputs)
                }
              }

            case Seq(dag) =>
              resolveDagNodeOrRef(dag.value, dag.inputs)
            case _ =>
              error(s"found more than a polymorphic factory for $typ ${describeId(id)}")
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
    
    private def implementsFunction(kind: Kind, funTyp:Type,  pos: Position, inp:Type, ret: Type): Dag[DagNode] = {
      val parName = TermName("par")
      val nd:Dag[DagNodeOrRef] = Leaf(DagNode.value(Kind.default, Nil, q"$parName",  inp, pos))
      val inpDag1 = Seq[(Id, Dag[DagNodeOrRef])] (nd.value.kind.id ->  nd)
      val inpts = resolveSeparate(ret, inpDag1) :: Nil
      
      Node(DagNode(
        ProviderSource.ValueSource,
        kind,
        funTyp.toString,
        _ => Nil,
        deps => {
          q"($parName:$inp) => ${deps.head}"
        },
        funTyp,
        pos), inpts)
    }

    private def implementsAbstractType(kind: Kind, typ: Type): Dag[DagNode] = {

      val primaryConstructor = membersSelect.getPrimaryConstructor(typ)
      val primaryConstrArgs = primaryConstructor.map(_.paramLists.flatten).getOrElse(Nil).map { par =>
        resolveDagNodeOrRef(Ref(Kind.default, par.info, par.pos), Nil)
      }

      val membersPairs = membersSelect.abstractMembers(typ).map(mthd => mthd -> implementMethod(mthd))

      Node(DagNode(
        ProviderSource.ValueSource,
        kind,
        typ.typeSymbol.fullName,
        _ => Nil,
        deps => {
          val (inputs, mthds) = deps.splitAt(primaryConstrArgs.length)
          val members = mthds.zip(membersPairs.map(_._1)).map {
            case (impl, mthd) =>
              val args = mthd.paramLists.map { pars =>
                pars.map { par =>
                  q"""${par.asTerm.name}: ${par.info}"""
                }
              }
              q"""def ${mthd.name}(...${args}):${mthd.returnType} = { $impl }"""
          }
          primaryConstructor match {
            case None => reflectUtils.newTrait(typ.typeSymbol, members)
            case Some(constructor) =>
              reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
          }
        },
        typ,
        typ.typeSymbol.pos), primaryConstrArgs ++ membersPairs.map(_._2))
    }

    private def implementMethod(m: MethodSymbol): Dag[DagNode] = {
      val parametersBindings: Seq[(Id, Dag[DagNodeOrRef])] = m.paramLists.flatMap { pars =>
        pars.flatMap { par =>
          val knd = kindProvider(par)
          knd.ids.map(id => id -> parameterDag(par, Kind(id, knd.scope)))
        }
      }
      val typ = m.returnType
      resolveSeparate(typ, parametersBindings)
    }

    private def resolveSeparate(typ: Type, parametersBindings: Seq[(Id, Dag[DagNodeOrRef])]): Dag[DagNode] = {
      val nmappings: Providers[DagNodeOrRef] = mappings.copy()
      nmappings ++= parametersBindings
      val dagProviders = this.dagProviders.copy

      new TypeResolver(nmappings, dagProviders, stack.clone()).resolveDagNodeOrRef(Ref(Kind(Global, DefaultScope), typ, typ.typeSymbol.pos), Nil)
    }

  }

}