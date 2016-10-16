package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait TypeResolverMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] =>

  import AbstractTypeDag._
  import DagToExpression.{ ConstructorCall, ImplementedMethod }

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
              if (definitions.FunctionClass.seq.contains(typ.typeSymbol)) {
                implementsFunction(ref.kind, typ, ref.sourcePos)
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
        s"allBindings$itemType",
        inps => Nil,
        inps => q"new com.github.gdefacci.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )",
        typ, pos,
        DagToExpression(inps => q"new com.github.gdefacci.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )"))

      resolveDagNode(nd, insts)
    }

    private def resolveDagNode(nd: DagNode, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = inputs match {
      case Seq() =>
        Leaf(nd)
      case inputs =>
        Node[DagNode](nd, inputs.map(dg => resolveDagNodeOrRef(dg.value, dg.inputs)))
    }

    //FunctionDagNodeFactory
    private def implementsFunction(kind: Kind, funTyp: Type, pos: Position): Dag[DagNode] = {

      val inpTypes = funTyp.typeArgs.init
      val parnames = 1.to(inpTypes.length).map(i => TermName(s"par$i"))
      val pars = inpTypes.zip(parnames).map { case (inp, parName) => q"$parName:$inp" }
      val parametersDags: Seq[Dag[DagNode]] = {
        inpTypes.zip(parnames).map { case (inp, par) => Leaf(DagNode.value(Kind.default, Nil, q"$par", inp, pos, DagToExpression.const(q"$par"))) }
      }
      val res = {
        val resultType = funTyp.typeArgs.last
        resolveSeparate(resultType, parametersDags.map(nd => nd.value.kind.id -> nd))
      }
      Node(new AbstractTypeDagNodeImpl1(kind, funTyp, _ => ???, _ => ???, DagToExpression.function(funTyp, pars.zip(parametersDags))), res :: Nil)
    }

    private def implementsFunction_old(kind: Kind, funTyp: Type, pos: Position): Dag[DagNode] = {
      val inpTypes = funTyp.typeArgs.init
      val resType = funTyp.typeArgs.last
      val parnames = 1.to(inpTypes.length).map(i => TermName(s"par$i"))
      val pars = inpTypes.zip(parnames).map { case (inp, parName) => q"$parName:$inp" }
      val nds = inpTypes.zip(parnames).map { case (inp, parName) => Leaf(DagNode.value(Kind.default, Nil, q"$parName", inp, pos, DagToExpression.const(q"$parName"))): Dag[DagNodeOrRef] }
      val inpDags = nds.map(nd => nd.value.kind.id -> nd)

      val inpts = resolveSeparate(resType, inpDags) :: Nil

      Node(DagNode(
        ProviderSource.ValueSource,
        kind,
        resType.typeSymbol.name.toString,
        funTyp.toString,
        _ => Nil,
        deps => {
          q"(..$pars) => ${deps.head}"
        },
        funTyp,
        pos, ???), inpts)
    }

    private def implementsAbstractType(kind: Kind, typ: Type): Dag[DagNode] = {

      val primaryConstructor = membersSelect.getPrimaryConstructor(typ)
      val constrPars = primaryConstructor.map(c => c -> c.paramLists.flatten).map {
        case (constr, pars) => new ConstructorCall(constr, pars.map(par => par -> resolveDagNodeOrRef(outboundParameterRef(Kind.default, par), Nil)))
      }
      val pars = membersSelect.abstractMembers(typ).map { mthd =>
        val (pdgs, impl) = implementMethod(mthd)
        new ImplementedMethod(mthd, pdgs, impl)
      }
      val dnf = new AbstractTypeDagNodeFactory(typ, constrPars, pars)
      Node(new AbstractTypeDagNodeImpl1(kind, typ, dnf.initializer, dnf.invoker, DagToExpression.abstractType(typ, constrPars, pars)),
        constrPars.map(_.parametersDags.map(_._2)).getOrElse(Nil) ++
          pars.map(_.impl))
    }

    private def inboundParameterDag(par: Symbol, knd: Kind): Dag[DagNode] =
      Leaf[DagNode](DagNode.value(knd, Nil, q"${par.asTerm.name}", par.info, par.pos, DagToExpression.const(q"${par.asTerm.name}")))

    private def implementMethod(m: MethodSymbol): (List[(Symbol, Set[Dag[DagNode]])], Dag[DagNode]) = {
      val pars0 = m.paramLists.flatMap { pars =>
        pars.map { par =>
          val knd = kindProvider(par)
          par -> knd.ids.map(id => id -> inboundParameterDag(par, Kind(id, knd.scope)))
        }
      }
      val pars = pars0.map {
        case (par, nds) => par -> nds.map(_._2)
      }
      val parametersBindings = pars0.flatMap(_._2)
      val typ = m.returnType
      pars -> resolveSeparate(typ, parametersBindings)
    }

    private def resolveSeparate(typ: Type, parametersBindings: Seq[(Id, Dag[DagNodeOrRef])]): Dag[DagNode] = {
      val nmappings: Providers[DagNodeOrRef] = mappings.copy()
      nmappings ++= parametersBindings
      val dagProviders = this.dagProviders.copy

      new TypeResolver(nmappings, dagProviders, stack.clone()).resolveDagNodeOrRef(Ref(Kind(Global, DefaultScope), typ, typ.typeSymbol.pos), Nil)
    }

  }

  //  class ConstructorCall(val constructor: MethodSymbol, val parametersDags: Seq[(Symbol, Dag[DagNode])])
  //  class ImplementedMethod(val method: MethodSymbol, val parametersDags: Seq[(Symbol, Set[Dag[DagNode]])], val impl: Dag[DagNode])

  private class AbstractTypeDagNodeFactory(
      typ: Type,
      constructorCall: Option[ConstructorCall],
      implementedMethods: Seq[ImplementedMethod]) {

    private def constructorAndMembersSplit[T](l: Seq[T]): (Seq[T], Seq[T]) =
      l.splitAt(constructorCall.map(_.parametersDags.length).getOrElse(0))

    private lazy val toInboundParameters = {
      val inboundParamsIds = implementedMethods.flatMap(_.parametersDags.flatMap(_._2.map(_.value.id))).toSet
      new DagConnections[DagNode](dg => inboundParamsIds.contains(dg.id))
    }

    private def isParamIndependentSingleton(d: Dag[DagNode]) =
      d.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(d)

    def initializer: Seq[DagToTree] => Seq[Tree] = (dependencies: Seq[DagToTree]) => {
      val (constrDeps, members) = constructorAndMembersSplit(dependencies)
      val singletonMembersDeps = DagToTree.distinct(members).filter(d => isParamIndependentSingleton(d.dag))
      DagToTree.distinct(constrDeps ++ singletonMembersDeps).flatMap(_.localInitialization)
    }

    def invoker: Seq[DagToTree] => Tree = (deps: Seq[DagToTree]) => {

      val (inputs, mthds) = constructorAndMembersSplit(deps)
      val members = mthds.zip(implementedMethods).map {
        case (impl, implMthd) =>
          val mthd = implMthd.method
          val args = mthd.paramLists.map { pars =>
            pars.map { par =>
              q"""${par.asTerm.name}: ${par.info}"""
            }
          }
          val localDecls = impl.allDependencies.filterNot(dt => isParamIndependentSingleton(dt.dag)).flatMap { d =>
            d.localInitialization
          }
          q"""def ${mthd.name}(...${args}):${mthd.returnType} = { 
                  ..$localDecls
                  ${impl.value} 
            }"""
      }
      constructorCall match {
        case None => reflectUtils.newTrait(typ.typeSymbol, members)
        case Some(constrCall) =>
          val constructor = constrCall.constructor
          reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs.map(_.value), members)
      }
    }

  }

  private class FunctionDagNodeFactory(funTyp: Type, pos: Position) {

    val inpTypes = funTyp.typeArgs.init
    val parnames = 1.to(inpTypes.length).map(i => TermName(s"par$i"))
    val pars = inpTypes.zip(parnames).map { case (inp, parName) => q"$parName:$inp" }
    val parametersDags: Seq[Dag[DagNode]] = {
      inpTypes.zip(parnames).map { case (inp, par) => Leaf(DagNode.value(Kind.default, Nil, q"$par", inp, pos, DagToExpression.const(q"$par"))) }
    }

    private lazy val toInboundParameters = {
      val inboundParamsIds = parametersDags.map(_.value.id)
      context.warning(pos, inboundParamsIds.mkString(", "))
      new DagConnections[DagNode](dg => inboundParamsIds.contains(dg.id))
    }

    private def isParamIndependentSingleton(d: Dag[DagNode]) = {
      //      val r = d.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(d)
      val r = !toInboundParameters.isConnected(d)
      context.warning(pos, ">>" + d.value.id + " ----- " + r + "--" + d.value)
      r
    }

    def initializer: Seq[DagToTree] => Seq[Tree] = (dependencies: Seq[DagToTree]) => {
      //        dependencies.head.allDependencies.filter(d => isParamIndependentSingleton(d.dag)).flatMap(_.localInitialization)
      //        DagToTree.distinct(dependencies.head.dependencies).filter(d => isParamIndependentSingleton(d.dag)).flatMap(_.localInitialization)

      Nil
    }

    def invoker: Seq[DagToTree] => Tree = (dependencies: Seq[DagToTree]) => {
      val v = dependencies.head
      val inits = v.allDependencies.filterNot(d => isParamIndependentSingleton(d.dag)).flatMap(_.localInitialization)
      q"""{ (..$pars) =>
        ..${v.initialization}
        ${v.value}
      }"""
    }

  }

}