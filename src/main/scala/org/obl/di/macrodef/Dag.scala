package org.obl.di.macrodef

private[di] sealed trait Dag[+T] {
  
  def value:T

  def inputs:Seq[Dag[T]]
  def visit:Seq[T] = Dag.visit(this)

}

private[di] final case class Leaf[T](value:T) extends Dag[T] {
  val inputs:Seq[Dag[T]] = Nil
}
private[di] final case class Node[T](value:T, inputs:Seq[Dag[T]]) extends Dag[T]

private[di] object Dag {

  import collection.mutable.{Set => MSet}

  def visit[T](d:Dag[T]):Seq[T] = {
    visitInternal(d, MSet.empty)
  }
  private def visitInternal[T](d:Dag[T], visited:MSet[T]):Seq[T] = {
    if (visited.contains(d.value)) Nil
    else {
      visited += d.value
      val inps = d match {
        case Leaf(_) => Nil
        case Node(_, inps) => inps
      }
      inps.flatMap( inp => visitInternal(inp, visited) ) :+ d.value
    }
  }

  import collection.mutable.{Map => MMap}

  def isConnectedTo[T](dag:Dag[T], pred:T => Boolean, current:MMap[Dag[T], Boolean]):Boolean = {
    current.get(dag) match {
      case Some(v) => v
      case None =>
        val r =
          if (pred(dag.value)) true
          else dag.inputs.exists(inp => isConnectedTo(inp, pred, current))
        current += (dag -> r)
        r
    }
  }
}



