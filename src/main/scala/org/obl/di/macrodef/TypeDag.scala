package org.obl.di.macrodef

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with TypeResolverMixin[C] with DagNodeOrRefFactory[C] with DagNodeFactory[C] with DagToExpr[C] {

  import context.universe._

  case class ModuleMappings(members: Seq[(Id, Dag[DagNodeOrRef])], polyMembers: Seq[(Id, DagNodeDagFactory)]) {
    def addMember(id: Id, d: Dag[DagNodeOrRef]) = copy(members = members :+ (id -> d))
    def addMembers(ms: Seq[(Id, Dag[DagNodeOrRef])]) = copy(members = members ++ ms)
    def addPolyMembers(ms: Seq[(Id, DagNodeDagFactory)]) = copy(polyMembers = polyMembers ++ ms)
  }
  
  def toDagNodesWithRefs(valueExpr: context.Expr[_], kindProvider: Symbol => Kinds): Providers[DagNodeOrRef] = {
    val exprTyp = valueExpr.actualType
    val exprNm = TermName(context.freshName(exprTyp.typeSymbol.name.decodedName.toString))
    val exprDag = alias(exprNm, valueExpr.tree, Kind.default)

    val membersMapping = membersSelect.getBindings(exprTyp).foldLeft(ModuleMappings(Nil, Nil)) {
      case (acc, membersSelect.MethodBinding(member)) =>
        val dgs = methodDag(exprDag, exprNm, member, kindProvider).toSeq.flatMap {
          case dg @ Leaf(dn: DagNode) => (dn.kind.id -> dg) :: Nil
          case dg @ Node(dn: DagNode, _) => (dn.kind.id -> dg) :: Nil
          case _ => Nil
        }
        acc.addMembers(dgs)
      case (acc, membersSelect.BindInstance(member, abstractType, concreteType)) =>
        val knds = kindProvider(member)
        if (concreteType.isAbstract) {
          context.abort(member.pos, s"The second type parameter of Bind must be a concrete class, ${concreteType} is not")
        }
        acc.addMembers(knds.ids.toSeq.map(id => id -> Leaf[DagNodeOrRef](Ref(Kind(id, knds.scope), concreteType.asType.toType, member.pos))))
      case (acc, membersSelect.PolyMethodBinding(member, polyType)) =>
        val knds = kindProvider(member)
        acc.addPolyMembers( knds.ids.toSeq.map(id => id -> new PolyDagNodeFactory(Kind(id, knds.scope), Some(exprNm), member, polyType, kindProvider ) ))
    }

    val allMappings = membersMapping.addMember(exprDag.value.kind.id, exprDag)

    MProvidersMap(allMappings.members, allMappings.polyMembers)
  }

  def instantiateObjectTree[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Tree = {
    val dag = instantiateDag(id, typ, mappings, kindProvider)

    val dag1 = Dag.update(dag) { (v, inps) =>
      val v1 =
        if (v.kind.scope != SingletonScope) v
        else
          DagNode(v.providerSource,
              v.kind,
            s"singleton${v.description}}",
            inps => Seq(q"""
                val ${v.singletonName} = ${v.invoke(inps)}
                """),
            inps => q"${v.singletonName}",
            v.typ,
            v.sourcePos,
            "singleton")
      if (inps.isEmpty) Leaf(v1) else Node(v1, inps)
    }

    val dagExpr = dagToExpr(dag1)
    context.typecheck(dagExpr.toTree)
  }

  def instantiateObject[T](id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Expr[T] = {
    context.Expr[T](instantiateObjectTree(id, typ, mappings, kindProvider))
  }
  
  import org.obl.di.graph

  def graphModel(id: Id,
    typ: Type,
    mappings: Providers[DagNodeOrRef],
    kindProvider: Symbol => Kinds): Tree = {
    val dag = instantiateDag(id, typ, mappings, kindProvider)
    val graphNodes = Dag.visit(dag).map(toDependencyTree)
    context.typecheck( q"List(..$graphNodes)" )
  }

  
  def toGraphType(typ:Type):Tree = typ.typeArgs match {
    case Seq() => q"org.obl.di.graph.TypeValue(${typ.typeSymbol.fullName.toString})"
    case args => 
      val typeArgs = args.map(toGraphType(_))
      q"org.obl.di.graph.PolymorphicType(${typ.erasure.typeSymbol.fullName}, List(..$typeArgs))"
  }
  
  def toDependencyTree(dag:Dag[DagNode]) = {
    val node = dag.value
    
    val graphScope = node.kind.scope match {
      case SingletonScope => q"org.obl.di.graph.DependencyScope.Singleton"
      case DefaultScope => q"org.obl.di.graph.DependencyScope.Factory"
    }
    val typ = toGraphType(node.typ)
    
    val providerSrc = dag.value.providerSource match {
      case ProviderSource.MethodSource(m) => q"org.obl.di.graph.MethodSource(${m.owner.fullName}, ${m.name.decodedName.toString})"
      case ProviderSource.ConstructorSource(c) => q"org.obl.di.graph.ConstructorSource(${c.owner.fullName})"
      case _ => q"org.obl.di.graph.ValueSource"
    }
    
    q"""
    org.obl.di.graph.Dependency(
        org.obl.di.graph.DependencyId(${node.id}), 
        $providerSrc,
        $graphScope, 
        $typ, 
        org.obl.di.graph.FilePosition(${node.sourcePos.source.file.path}, ${node.sourcePos.line}), 
        List(..${dag.inputs.map( i => q"org.obl.di.graph.DependencyId(${i.value.id})" ) }) )
    """
  }
  
  
}