package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodes[C <: Context] {
  val context: C

  import context.universe._

  val reflectUtils = new ReflectUtils[context.type](context)
  val scopeProvider = new DagScopeProvider[context.type](context)

  private object IdGen {
    private var counter = 0
    def next = {
      counter += 1
      counter
    }
  }

  sealed trait DagNodeOrRef {
    def scope: DagScope
    def typ: Type
    def sourcePos: Position
    def description: String
    override def toString = description
  }

  sealed case class Ref(val scope: DagScope, val typ: Type, val sourcePos: Position) extends DagNodeOrRef {
    assert(typ != null)
    def description: String = s"Reference to type $typ"
  }

  val Gen = new GenModel[Dag[DagNode], TermName, Tree]

  def genExpressionToTree(expr: Gen.Expression): Tree = {
    expr match {
      case v: Gen.Value => v.value
      case v: Gen.Block =>
        val decls = v.declarations.map { decl =>
          val nm = decl.name
          val v = genExpressionToTree(decl.expression)
          q"val $nm = $v"
        }
        q"""
        ..$decls
        ${v.value}
        """
    }
  }

  case class Decorator(inputs: Seq[Dag[DagNodeOrRef]], containerTermName: TermName, method: MethodSymbol, selfIndex: Int)

  type DagToExpression = (Dag[DagNode], Seq[Dag[Gen.Expression]]) => Gen.Expression

  object DagToExpression {

    def apply(termName: TermName, value: Tree): DagToExpression = { (dag, deps) =>
      val decls = Gen.allDeclarations(deps.map(_.value), _.source.value.id)

      val v = new Gen.Value(dag, value)
      val decl = new Gen.Declaration(termName, v)
      new Gen.Block(dag, decls :+ decl, q"$termName")
    }

    def const(tree: Tree): DagToExpression = { (dag, deps) =>
      new Gen.Value(dag, tree)
    }

    def apply(valuef: Seq[Tree] => Tree): DagToExpression = { (dag, deps) =>
      val decls = Gen.allDeclarations(deps.map(_.value), _.source.value.id)
      singletonize(new Gen.Block(dag, decls, valuef(deps.map(_.value.value))))
    }

    def singletonize(e: Gen.Expression): Gen.Expression = {
      if (e.source.value.scope != ApplicationScope) e
      else {
        val singletonName = TermName(context.freshName("singleton" + e.source.value.name))
        e match {
          case v: Gen.Value =>
            new Gen.Block(v.source, new Gen.Declaration(singletonName, v) :: Nil, q"$singletonName")
          case v: Gen.Block =>
            val v1 = v.source.value
            val newSource =
              DagNode(v1.providerSource, v1.scope, v1.description, v1.name, v1.typ, v1.sourcePos, DagToExpression.const(q""))

            new Gen.Block(v.source, v.declarations :+ new Gen.Declaration(singletonName, new Gen.Value(Dag(newSource, e.source.inputs), v.value)), q"$singletonName")
        }
      }
    }

  }

  sealed abstract case class DagNode(id: Int) extends DagNodeOrRef {
    def name: String
    def providerSource: ProviderSource

    def dagToExpression: DagToExpression
  }

  sealed trait ProviderSource

  object ProviderSource {

    case class MethodSource(method: MethodSymbol) extends ProviderSource
    case class DecoratorSource(method: MethodSymbol) extends ProviderSource
    case class ConstructorSource(method: MethodSymbol) extends ProviderSource
    case object ValueSource extends ProviderSource
    case class AllbindingsSource(itemType: TypeSymbol) extends ProviderSource
  }

  object DagNode {

    private final class DagNodeImpl private[DagNode] (
        val providerSource: ProviderSource,
        val scope: DagScope,
        val name: String,
        val description: String,
        val typ: Type,
        val sourcePos: Position,
        val dagToExpression: DagToExpression) extends DagNode(IdGen.next) {

      assert(typ != null, s"could not infer type on tree $description")

    }

    def apply(
      providerSource: ProviderSource,
      scope: DagScope,
      name: String,
      description: String,
      typ: Type,
      sourcePos: Position,
      dagToExpression: DagToExpression): DagNode = new DagNodeImpl(providerSource, scope, name, description, typ, sourcePos, dagToExpression)

    def value(scope: DagScope, value: Tree, typ: Type, sourcePos: Position, dagToExpression: DagToExpression): DagNode =
      apply(ProviderSource.ValueSource, scope, s"$value", s"$value", typ, sourcePos, dagToExpression)

    def methodCall(scope: DagScope, containerTermName: Option[TermName], method: Symbol): DagNode = {
      val methodSymbol = method.asMethod
      val providerSource = new ProviderSource.MethodSource(method.asMethod)
      val mthdName = method.name.decodedName.toString
      val description = s"${method.owner.name}.${method.name}"
      apply(providerSource, scope, mthdName, description,
        typ = methodSymbol.returnType,
        sourcePos = method.pos,
        DagToExpression.apply(inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs)))
    }

    def constructorCall(scope: DagScope, typ: Type, constructor: MethodSymbol, members: Seq[Tree]): DagNode = {
      val invoker: Seq[Tree] => Tree = if (members.isEmpty) { inputs =>
        reflectUtils.methodCall(None, constructor, inputs)
      } else { inputs =>
        reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      val providerSource = new ProviderSource.ConstructorSource(constructor)
      val typName = typ.typeSymbol.name.decodedName.toString
      apply(providerSource, scope, typName, s"$constructor", typ, constructor.pos,
        DagToExpression(invoker))
    }

  }

}
