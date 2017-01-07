package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait TypeResolverMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpressionFactoryMixin[C] =>

  import DagToExpressionFactory.{ ConstructorCall, ImplementedMethod }

  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)

  class Stacker private (stack: IndexStack[DagNodeOrRef]) {

    def message: String = {
      stack.map(_.typ).distinct.map(hd => s"required by ${hd}").mkString("\n")
    }

    def error(msg: String) = {
      context.abort(context.enclosingPosition, msg +" "+ this.message)
    }

    def withStack(nd: DagNodeOrRef)(res: => Dag[DagNode]) = {
      stack.pushIfNotDuplicated(nd, () => {
        val typ = nd.typ
        error(s"cycle detected with type $typ")        
      })
      val r = res
      stack.pop
      r
    }

    def copy() = new Stacker(stack.copy())

  }
  
  object Stacker {
    def empty = new Stacker(new IndexStack[DagNodeOrRef])
  }
  
  import scala.collection.mutable.Buffer
  
  class TypeResolver(
      mappings: Providers[DagNodeOrRef],
      dagProviders: Buffer[Dag[DagNode]],
      topLevelRefs: Set[Ref],
      stack: Stacker = Stacker.empty) {

    private val currentSingletons: Buffer[Dag[DagNode]] = Buffer.empty

    private def error(msg: String) = {
      stack.error(msg)
    }
    
    private def missingBindingError(typ: Type) = {
      error(s"could not find a binding for $typ")
    }

   
    def decorate(res:Dag[DagNode]) = {
      val decs = mappings.getDecorators(res.value.typ weak_<:< _)
      if (decs.size > 1) error("found more than a decorator for type "+res.value.typ)
      else {
        decs.headOption.map { dec =>
          Dag(DagNode(ProviderSource.DecoratorSource(dec.method), res.value.scope, res.value.name+"Decorator", res.value.description+"Decorator", 
              res.value.typ, res.value.sourcePos, DagToExpressionFactory.decorator(dec.containerTermName, dec.method, dec.selfIndex)),
              res :: dec.inputs.map { dg => 
                resolveDagNodeOrRef(dg.value, dg.inputs) 
            }.toList )
        } .getOrElse {
          res
        }
      }
    }
    
    private def resolveDagNodeOrRef(nd: DagNodeOrRef, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      nd match {
        case ref @ Ref(_, _, _) =>
          assert(inputs.isEmpty, "refs must have no inputs")
          resolveRef(ref)
        case nd: DagNode => 
          resolveDagNode(nd, inputs)
      }
    }

    def resolveRef(ref: Ref): Dag[DagNode] = {
      val Ref(_, typ, _) = ref

      val rs = dagProviders.filter(nd => nd.value != ref && nd.value.typ <:< typ)
      rs match {
        case Seq() =>
          val r = membersSelect.multiTargetItem(typ) match {

            case Some(itemType) => resolveMultiTargetRef(ref, itemType)
            case None => resolveTargetRef(ref)

          }
          if (dagProviders.find(nd => nd.value.typ <:< typ).isEmpty) {
            if (r.value.scope == ApplicationScope) currentSingletons += r
            dagProviders += r
          }
          r
        case Seq(hd) => hd
        case items => error(s"more than one value of $typ: ${items.map(_.value).mkString(", ")}")

      }
    }

    private def resolveTargetRef(ref: Ref): Dag[DagNode] = {
      val Ref(_, typ, pos) = ref
      val typMappings = mappings.findMembers { nd => nd != ref && nd.typ <:< typ }
      typMappings match {
        case Seq() =>
          val polyMembers = mappings.findPolymorphicMembers(df => df.apply(typ))
          polyMembers match {
            case Seq() => instantiateRef(ref)
            case Seq(dag) => resolveDagNodeOrRef(dag.value, dag.inputs)
            case _ => error(s"found more than a polymorphic factory for $typ")
          }

        case Seq(dag) => resolveDagNodeOrRef(dag.value, dag.inputs)
        case _ => error(s"found more than a value for $typ")
      }
    }

    private def resolveMultiTargetRef(ref: Ref, itemType: Type):Dag[DagNode] = {
      val Ref(_, typ, pos) = ref
      val insts: Seq[Dag[DagNode]] = 
        mappings.findMembers((nd) => nd != ref && nd.typ <:< itemType).map( dg => resolveDagNodeOrRef(dg.value, dg.inputs))
      val desc = s"allBindings$itemType"
      Dag( DagNode(new ProviderSource.AllbindingsSource(itemType.typeSymbol.asType),
        ref.scope,
        desc, desc,
        typ, pos,
        DagToExpression(inps => q"new com.github.gdefacci.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )")), insts)
    }

    private def resolveDagNode(nd: DagNode, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      stack.withStack(nd) {
        Dag[DagNode](nd, inputs.map(dg => resolveDagNodeOrRef(dg.value, dg.inputs)))
      }
    }

    private def instantiateRef(ref: Ref):Dag[DagNode] = {
      val Ref(_, typ, pos) = ref

      lazy val noBindingError = missingBindingError(typ)

      if (membersSelect.isPrimitive(typ)) {
        noBindingError
      }

      stack.withStack(ref) {
        lazy val isTopLevelref = topLevelRefs.contains(ref)

        if (reflectUtils.isFunctionType(typ.typeSymbol)) {
          if (isTopLevelref) implementsFunction(ref.scope, typ, ref.sourcePos)
          else noBindingError
        } else if (typ.typeSymbol.isAbstract) {
          if (isTopLevelref) implementsAbstractType(ref.scope, typ)
          else noBindingError
        } else {
          val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
            error(s"cant find primary constructor for ${typ.typeSymbol.fullName} resolving $typ")
          }
          membersSelect.getPolyType(constructorMethod.returnType.etaExpand).map { polyType =>
            val dg = new PolyDagNodeFactory(ref.scope, None, constructorMethod, polyType).apply(ref.typ).getOrElse {
              error(s"error creating dag for polymorpic primary constructor for ${typ.typeSymbol.fullName} resolving $typ")
            }
            resolveDagNodeOrRef(dg.value, dg.inputs)
          }.getOrElse {
            val dnd = constructorDag(ref.scope, typ, constructorMethod, Nil)
            resolveDagNodeOrRef(dnd.value, dnd.inputs)
          }
        }
      }
    }

    private def implementsFunction(scope: DagScope, funTyp: Type, pos: Position): Dag[DagNode] = {

      val inpTypes = funTyp.typeArgs.init
      val parnames = 1.to(inpTypes.length).map(i => TermName(s"par$i"))
      val pars = inpTypes.zip(parnames).map { case (inp, parName) => q"$parName:$inp" }
      val parametersDags: Seq[Dag[DagNode]] = {
        inpTypes.zip(parnames).map { case (inp, par) => Dag(DagNode.value(DefaultScope, q"$par", inp, pos, DagToExpression.const(q"$par"))) }
      }
      val res = {
        val resultType = funTyp.typeArgs.last
        resolveSeparate(resultType, parametersDags)
      }
      val ftsym = funTyp.typeSymbol
      val name: String = ftsym.name.toString
      val description = ftsym.fullName
      Dag(DagNode(ProviderSource.ValueSource, scope, name, description, funTyp, ftsym.pos, DagToExpressionFactory.function(funTyp, pars.zip(parametersDags))),
        res :: Nil)
    }

    private def implementsAbstractType(scope: DagScope, typ: Type): Dag[DagNode] = {

      val primaryConstructor = membersSelect.getPrimaryConstructor(typ)
      val constrPars = primaryConstructor.map(c => c -> c.paramLists.flatten).map {
        case (constr, pars) => new ConstructorCall(constr, pars.map(par => par -> resolveDagNodeOrRef(outboundParameterDag(DefaultScope, par), Nil)))
      }
      val pars = membersSelect.abstractMembers(typ).map { mthd =>
        val (pdgs, impl) = implementMethod(mthd)
        new ImplementedMethod(mthd, pdgs.map { case (k,v) => k -> Set(v) }, impl)
      }
      val tsym = typ.typeSymbol
      val name: String = tsym.name.toString
      val description = tsym.fullName
      val pos = tsym.pos

      Dag(DagNode(ProviderSource.ValueSource, scope, name, description, typ, pos, DagToExpressionFactory.abstractType(typ, constrPars, pars)),
        constrPars.map(_.parametersDags.map(_._2)).getOrElse(Nil) ++ pars.map(_.impl))
    }

    private def inboundParameterDag(par: Symbol, knd: DagScope): Dag[DagNode] =
      Dag[DagNode](DagNode.value(knd, q"${par.asTerm.name}", par.info, par.pos, DagToExpression.const(q"${par.asTerm.name}")))

    private def implementMethod(m: MethodSymbol): (List[(Symbol, Dag[DagNode])], Dag[DagNode]) = {
      val pars0 = m.paramLists.flatMap { pars =>
        pars.map { par =>
          val knd = scopeProvider(par)
          par -> inboundParameterDag(par, knd)
        }
      }
      val pars = pars0.map {
        case (par, nds) => par -> nds
      }
      val parametersBindings = pars0.map(_._2) 
      val typ = m.returnType
      pars -> resolveSeparate(typ, parametersBindings)
    }

    private def resolveSeparate(typ: Type, parametersBindings: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      val nmappings: Providers[DagNodeOrRef] = mappings.copy()
      nmappings.members ++= parametersBindings
      val dagProviders = this.dagProviders.clone

      val tr = new TypeResolver(nmappings, dagProviders, topLevelRefs, stack.copy())
      val res = tr.resolveDagNodeOrRef(Ref(DefaultScope, typ, typ.typeSymbol.pos), Nil)
      this.dagProviders ++= tr.currentSingletons
      res
    }

  }

}