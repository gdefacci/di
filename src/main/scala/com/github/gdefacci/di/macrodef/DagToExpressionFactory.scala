package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

trait DagToExpressionFactoryMixin[C <: Context] { self: DagNodes[C] =>
  val context: C

  import context.universe._
  
  import DagToExpression.singletonize

  object DagToExpressionFactory {
    
    private def createConnections(ids:Set[Int]) = 
      new DagConnections[DagNode, Int]({ dg => ids.contains(dg.id) }, _.id)
      
    private def createExpression(dag:Dag[DagNode], decls:Seq[Gen.Declaration], tree:Tree) =
      decls match {
        case Seq() => new Gen.Value(dag, tree)
        case decls => new Gen.Block(dag, decls, tree)
      }

    def function(funTyp: Type, paramsDags: Seq[(Tree, Dag[DagNode])]): DagToExpression = {
      lazy val toInboundParameters = createConnections(paramsDags.map(_._2.value.id).toSet)
      
      val isParamIndependentSingleton = (expr: Gen.Expression) => {
        expr.source.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(expr.source)
      }

      return { (dag, deps) =>
        assert(deps.length == 1)

        val res = deps.head

        val (impl, decls) = Gen.partitionContent(expr => !isParamIndependentSingleton(expr))(res.value)
        val pars = paramsDags.map(_._1)
        val funTree = q"(..$pars) => { ${genExpressionToTree(impl)} }"
        
        singletonize(createExpression(dag, decls, funTree))
      }
    }
    
    class ConstructorCall(val constructor: MethodSymbol, val parametersDags: Seq[(Symbol, Dag[DagNode])])
    class ImplementedMethod(val method: MethodSymbol, val parametersDags: Seq[(Symbol, Set[Dag[DagNode]])], val impl: Dag[DagNode])

    def abstractType(typ: Type,
      constructorCall: Option[ConstructorCall],
      implementedMethods: Seq[ImplementedMethod]): DagToExpression = {

      lazy val toInboundParameters = createConnections(implementedMethods.flatMap(_.parametersDags.flatMap(_._2.map(_.value.id))).toSet)

      val isParamIndependentSingleton = (d: Dag[DagNode]) => {
        d.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(d)
      }

      val constrParLen = constructorCall.map(_.parametersDags.length).getOrElse(0)
      
      return { (dag, deps) =>

        val (constrDeps, membDeps) = deps.splitAt(constrParLen)

        val members = membDeps.zip(implementedMethods).map {
          case (bodyExpr, implMthd) =>
            val mthd = implMthd.method
            val args = mthd.paramLists.map { pars =>
              pars.map { par => q"${par.asTerm.name}: ${par.info}" }
            }
            val (impl, decls) = Gen.partitionContent(expr => !isParamIndependentSingleton(expr.source))(bodyExpr.value)
            val body = genExpressionToTree(impl)
            val mthdTree = q"def ${mthd.name}(...${args}):${mthd.returnType} = { $body }"

            createExpression(dag, decls, mthdTree)
        }

        val allDecls = Gen.allDeclarations(constrDeps.map(_.value) ++ members, _.source.value.id)
        val resTree = constructorCall match {
          case None => 
            reflectUtils.newTrait(typ.typeSymbol, members.map(_.value))
          case Some(constrCall) =>
            val constructor = constrCall.constructor
            reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, constrDeps.map(_.value.value), members.map(_.value))
        }
        
        singletonize(createExpression(dag, allDecls, resTree))
      }
    }
  }

}