package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

class DagGraph[C <: Context](val context: C) {

  import context.universe._
  
  val td = new TypeDag[context.type](context)

  import org.obl.di.graph

  def graphModel(id: Id,
    typ: Type,
    mappings: td.Providers[td.DagNodeOrRef],
    kindProvider: Symbol => Kinds): Tree = {
    val dag = td.instantiateDag(id, typ, mappings, kindProvider)
    val graphNodes = Dag.visit(dag).map(toDependencyTree)
    context.typecheck(q"List(..$graphNodes)")
  }

  def toGraphType(typ: Type): Tree = typ.typeArgs match {
    case Seq() => q"org.obl.di.graph.TypeValue(${typ.typeSymbol.fullName.toString})"
    case args =>
      val typeArgs = args.map(toGraphType(_))
      q"org.obl.di.graph.PolymorphicType(${typ.erasure.typeSymbol.fullName}, List(..$typeArgs))"
  }

  def toDependencyTree(dag: Dag[td.DagNode]) = {
    val node = dag.value

    val graphScope = node.kind.scope match {
      case SingletonScope => q"org.obl.di.graph.DependencyScope.Singleton"
      case DefaultScope => q"org.obl.di.graph.DependencyScope.Factory"
    }
    val typ = toGraphType(node.typ)

    val providerSrc = dag.value.providerSource match {
      case td.ProviderSource.MethodSource(m) => q"org.obl.di.graph.MethodSource(${m.owner.fullName}, ${m.name.decodedName.toString})"
      case td.ProviderSource.ConstructorSource(c) => q"org.obl.di.graph.ConstructorSource(${c.owner.fullName})"
      case _ => q"org.obl.di.graph.ValueSource"
    }

    q"""
    org.obl.di.graph.Dependency(
        org.obl.di.graph.DependencyId(${node.id}), 
        $providerSrc,
        $graphScope, 
        $typ, 
        org.obl.di.graph.FilePosition(${node.sourcePos.source.file.path}, ${node.sourcePos.line}), 
        List(..${dag.inputs.map(i => q"org.obl.di.graph.DependencyId(${i.value.id})")}) )
    """
  }
}