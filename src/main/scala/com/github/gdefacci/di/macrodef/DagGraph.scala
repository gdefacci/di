package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

class DagGraph[C <: Context](val context: C) {

  import context.universe._
  
  val td = new TypeDag[context.type](context)

  import com.github.gdefacci.di.graph

  def graphModel(id: Id,
    typ: Type,
    mappings: td.Providers[td.DagNodeOrRef]): Tree = {
    val dag = td.instantiateDag(id, typ, mappings)
    val graphNodes = Dag.visit(dag).map(toDependencyTree)
    context.typecheck(q"List(..$graphNodes)")
  }

  def toGraphType(typ: Type): Tree = typ.typeArgs match {
    case Seq() => q"com.github.gdefacci.di.graph.TypeValue(${typ.typeSymbol.fullName.toString})"
    case args =>
      val typeArgs = args.map(toGraphType(_))
      q"com.github.gdefacci.di.graph.PolymorphicType(${typ.erasure.typeSymbol.fullName}, List(..$typeArgs))"
  }

  def toDependencyTree(dag: Dag[td.DagNode]) = {
    val node = dag.value

    val graphScope = node.kind.scope match {
      case SingletonScope => q"com.github.gdefacci.di.graph.DependencyScope.Singleton"
      case DefaultScope => q"com.github.gdefacci.di.graph.DependencyScope.Factory"
    }
    val typ = toGraphType(node.typ)

    val providerSrc = dag.value.providerSource match {
      case td.ProviderSource.MethodSource(m) => q"com.github.gdefacci.di.graph.MethodSource(${m.owner.fullName}, ${m.name.decodedName.toString})"
      case td.ProviderSource.ConstructorSource(c) => q"com.github.gdefacci.di.graph.ConstructorSource(${c.owner.fullName})"
      case _ => q"com.github.gdefacci.di.graph.ValueSource"
    }

    q"""
    com.github.gdefacci.di.graph.Dependency(
        com.github.gdefacci.di.graph.DependencyId(${node.id}), 
        $providerSrc,
        $graphScope, 
        $typ, 
        com.github.gdefacci.di.graph.FilePosition(${node.sourcePos.source.file.path}, ${node.sourcePos.line}), 
        List(..${dag.inputs.map(i => q"com.github.gdefacci.di.graph.DependencyId(${i.value.id})")}) )
    """
  }
}