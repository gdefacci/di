package com.github.gdefacci.di.macrodef

private[di] sealed trait Dag[+T] {

  def value: T
  def inputs: Seq[Dag[T]]

}

private[di] final case class Leaf[T](value: T) extends Dag[T] {
  val inputs: Seq[Dag[T]] = Nil
}
private[di] final case class Node[T](value: T, inputs: Seq[Dag[T]]) extends Dag[T]

private[di] object Dag {

  import collection.mutable.{ Set => MSet, Map => MMap }

  def update[T](dag: Dag[T])(f: (T, Seq[Dag[T]]) => Dag[T]): Dag[T] = {
    update(dag, f, collection.mutable.Map.empty[T, Dag[T]])
  }

  private def update[T](dag: Dag[T], f: (T, Seq[Dag[T]]) => Dag[T], mmap: MMap[T, Dag[T]]): Dag[T] = {
    mmap.get(dag.value) match {
      case Some(v) => v
      case None =>
        val r = dag match {
          case Leaf(v) => f(v, Nil)
          case Node(v, inps) => {
            val dinps = inps.map(update(_, f, mmap))
            f(v, dinps)
          }
        }
        mmap += (dag.value -> r)
        r
    }
  }

  def visit[T](d: Dag[T]): Seq[Dag[T]] = {
    visitDagInternal(d, MSet.empty)
  }
  
  def fold[T, ACC](dag:Dag[T], zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    val z1 = dag.inputs.foldLeft(zero) { (acc, i) =>
      fold(i, acc)(f)
    }
    f(z1, dag)
  }
  
  def mapValues[A, ID, B](d:Dag[A], keyf:A => ID)(f:(Dag[A], Seq[B]) => B) = {
    val mp = fold[A, Map[ID, B]](d, Map.empty) { (acc, dg) =>
      val k = keyf(dg.value)
      acc.get(k) match {
        case Some(_) => acc
        case None =>
          val v = f(dg, dg.inputs.map { dg =>
            acc(keyf(dg.value))
          })
          acc + (k -> v)
      }
    }
    mp(keyf(d.value))
  }
  
  def foldDistinct[T, ID, ACC](dag:Dag[T], keyf:T => ID, zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    fold(dag, Set.empty[ID] -> zero) { (acc0, i) => 
      val (visited, acc) = acc0
      val k= keyf(i.value)
      if (visited.contains(k)) acc0
      else (visited + k) -> f(acc, i)        
    }._2
  }

  private def visitDagInternal[T](d: Dag[T], visited: MSet[T]): Seq[Dag[T]] = {
    if (visited.contains(d.value)) Nil
    else {
      visited += d.value
      val inps = d match {
        case Leaf(_) => Nil
        case Node(_, inps) => inps
      }
      inps.flatMap(inp => visitDagInternal(inp, visited)) :+ d
    }
  }

  /**
   * FIXME not used
   */
  def isConnectedTo[T](dag: Dag[T], pred: T => Boolean, current: MMap[Dag[T], Boolean]): Boolean = {
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



