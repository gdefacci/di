package com.github.gdefacci.di.macrodef

import collection.mutable.{Set => MSet, Buffer}

private[di] object ProvidersMap {

  def empty[T, TF, RF, TYP, DEC] = new ProvidersMap[T, TF, RF, TYP, DEC](Buffer.empty, Buffer.empty, MSet.empty, Buffer.empty)
  
  def apply[T, TF, RF, TYP, DEC](mp: Seq[Dag[T]], polyMembersMap: Seq[TF], topLevelRefs: Set[RF], decoratorsMap:Seq[(TYP, DEC)]): ProvidersMap[T, TF, RF, TYP, DEC] = {
    val res = empty[T, TF, RF, TYP, DEC]
    res.decoratorsBuffer ++= decoratorsMap
    
    res.members ++= mp
    res.polyMembers ++= polyMembersMap
    res.topLevelRefsSet ++= topLevelRefs
    res
  }
}

private[di] class ProvidersMap[T, TF, RF, TYP, DEC] private (
    val members: Buffer[Dag[T]],
    val polyMembers: Buffer[TF],
    val topLevelRefsSet: MSet[RF],
    val decoratorsBuffer:Buffer[(TYP, DEC)]) {

  def getDecorators(pred:TYP => Boolean):Seq[DEC] = {
    decoratorsBuffer.collect { case (k,v) if pred(k) => v }.toSeq
  }
  
  def findMembers(typPredicate: T => Boolean): Seq[Dag[T]] =
    members.filter((dag) => typPredicate(dag.value))

  def findPolymorphicMembers[R](select: TF => Option[R]): Seq[R] =
    polyMembers.map(select).collect { case Some(v) => v }

  def ++=(e: ProvidersMap[T, TF, RF, TYP, DEC]): Unit = {
    members ++= e.members
    polyMembers ++= e.polyMembers 
    topLevelRefsSet ++= e.topLevelRefsSet
    decoratorsBuffer ++= e.decoratorsBuffer;
  }

  def topLevelRefs: Set[RF] = topLevelRefsSet.toSet
  def decorators: Seq[(TYP, DEC)] = decoratorsBuffer

  def copy() = new ProvidersMap(members.clone(), polyMembers.clone(), topLevelRefsSet.clone(), decoratorsBuffer.clone())
}

