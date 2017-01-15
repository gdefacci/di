package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

class DagGraph[C <: Context](val context: C) {

  import context.universe._

  val typeDag = new TypeDag[context.type](context)

  import com.github.gdefacci.di.graph

  def graphModel(typ: Type,
    mappings: typeDag.Providers[typeDag.DagNodeOrRef]): Tree = {
    
    val membersSelect = new MembersSelect[context.type](context)
    
    val typeResolver = new typeDag.TypeResolver(membersSelect, mappings, collection.mutable.Buffer.empty, mappings.topLevelRefs)

    val dag = typeResolver.resolveRef(typeDag.Ref(DefaultScope, typ, typ.typeSymbol.pos))

    val graphNodes = dag.visit.map(toDependencyTree)
    context.typecheck(q"List(..$graphNodes)")
  }

  private def containers(s: Symbol): List[Symbol] = {
    if (s == NoSymbol) Nil
    else if (s == context.mirror.RootClass) Nil
    else s :: containers(s.owner)
  }

  private def toTypeOwner(syms: Seq[Symbol]): Tree = {
    syms match {
      case Seq() => q"com.github.gdefacci.di.graph.Package(Nil)"
      case init :+ last =>
        if (last.isPackage) {
          val segs = syms.map(_.name.toTermName.toString)
          q"com.github.gdefacci.di.graph.Package(List(..$segs))"
        } else {
          val typ = last.asType.toType
          toGraphType(typ)
        }
    }
  }

  private def toGraphType(typ: Type): Tree = {
    val owner = toTypeOwner(containers(typ.typeSymbol.owner).reverse)
    if (typ.typeSymbol.isModuleClass) {
      q"com.github.gdefacci.di.graph.SingletonTypeValue($owner, ${typ.typeSymbol.name.toTermName.toString})"
    } else typ.typeArgs match {
      case Seq() =>
        q"com.github.gdefacci.di.graph.TypeValue($owner, ${typ.typeSymbol.name.toTermName.toString})"
      case args =>
        val typeArgs = args.map(toGraphType)
        q"com.github.gdefacci.di.graph.PolymorphicType($owner, ${typ.erasure.typeSymbol.name.toTermName.toString}, List(..$typeArgs))"
    }
  }
  
  private def toDependencyTree(dag: Dag[typeDag.DagNode]) = {
    val node = dag.value

    val graphScope = node.scope match {
      case ApplicationScope => q"com.github.gdefacci.di.graph.DependencyScope.Singleton"
      case DefaultScope => q"com.github.gdefacci.di.graph.DependencyScope.Factory"
    }
    val typ = toGraphType(node.typ)

    val providerSrc = dag.value.providerSource match {
      case typeDag.ProviderSource.MethodSource(m) => q"com.github.gdefacci.di.graph.MethodSource(${m.owner.fullName}, ${m.name.decodedName.toString})"
      case typeDag.ProviderSource.DecoratorSource(m) => q"com.github.gdefacci.di.graph.DecoratorSource(${m.owner.fullName}, ${m.name.decodedName.toString})"
      case typeDag.ProviderSource.ConstructorSource(c) => q"com.github.gdefacci.di.graph.ConstructorSource(${c.owner.fullName})"
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