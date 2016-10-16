package com.github.gdefacci.di.macrodef

class GenModel[S, N, V] {

  sealed trait Expression {
    def source: S
    def value: V
  }

  final class Block(
    val source: S,
    val declarations: Seq[Declaration],
    val value: V) extends Expression {
    override def toString = s"Block($source, ${declarations.mkString(",")}, $value)"
  }


  final class Declaration(val name: N, val expression: Expression) {
    override def toString = s"Declaration($name = $expression)"
  }
  final class Value(val source: S, val value: V) extends Expression {
    override def toString = s"Value($source, $value)"
  }

  def fromDag[T, ID](keyf: T => ID, f: (Dag[T], Seq[Dag[Expression]]) => Expression)(dag: Dag[T]): Dag[Expression] = {
    Dag.mapValues[T, ID, Dag[Expression]](dag, keyf) { (d, inpDags) =>
      Node(f(d, inpDags), inpDags)
    }
  }

  def allDeclarations[ID](deps: Seq[Expression], keyf: Expression => ID) =
    deps.foldLeft(Set.empty[ID] -> Seq.empty[Declaration]) { (acc, mbr) =>
      val (visited, res) = acc
      val id = keyf(mbr)
      if (visited.contains(id)) acc
      else {
        val decls = mbr match {
          case v: Value => Nil
          case b: Block => b.declarations
        }
        decls.foldLeft(visited -> res) { (acc1, decl) =>
          val (visited, res) = acc1
          val id1 = keyf(decl.expression)
          if (visited.contains(id1)) acc1
          else {
            (visited + id1) -> (res :+ decl)
          }
        }
      }
    }._2
  
  private def partitionElements(exprs: Seq[Declaration], pred: Expression => Boolean): (Seq[Declaration], Seq[Declaration]) = {
    exprs.foldLeft(Seq.empty[Declaration] -> Seq.empty[Declaration]) { (acc, decl) =>
      val (sat, skip) = acc
      val (nsat, nskip) = partitionContent(pred)(decl.expression)

      (sat :+ new Declaration(decl.name, nsat)) -> (skip ++ nskip)
    }
  }

  def partitionContent(pred: Expression => Boolean)(expr: Expression): (Expression, Seq[Declaration]) = {
    expr match {
      case d: Value => d -> Nil
      case bl: Block =>
        val decls = bl.declarations.partition(decl => pred(decl.expression))
        val (sat, skip) = partitionElements(decls._1, pred)
        new Block(bl.source, sat, bl.value) -> (decls._2 ++ skip)
//      case bl: FunctionScope =>
//        val decls = bl.declarations.partition(decl => pred(decl.expression))
//        val (sat, skip) = partitionElements(decls._1, pred)
//        new FunctionScope(bl.source, bl.inbound, sat, bl.value) -> (decls._2 ++ skip)
    }
  }

}