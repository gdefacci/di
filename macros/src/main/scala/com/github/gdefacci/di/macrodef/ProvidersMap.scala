package com.github.gdefacci.di.macrodef

import collection.mutable.{Set => MSet, Buffer}

private[di] object ProvidersMap {

  def empty[T, TF, RF, TYP, DEC, PDEC] = new ProvidersMap[T, TF, RF, TYP, DEC, PDEC](Buffer.empty, Buffer.empty, MSet.empty, Buffer.empty, Buffer.empty)
  
  def apply[T, TF, RF, TYP, DEC, PDEC](mp: Seq[Dag[T]], polyMembers: Seq[TF], topLevelRefs: Set[RF], decoratorsMap:Seq[(TYP, DEC)], polyDecorators: Seq[TF]): ProvidersMap[T, TF, RF, TYP, DEC, PDEC] = {
    val res = empty[T, TF, RF, TYP, DEC, PDEC]
    res.decoratorsBuffer ++= decoratorsMap
    
    res.members ++= mp
    res.polyMembers ++= polyMembers
    res.topLevelRefsSet ++= topLevelRefs
    res
  }
}

private[di] class ProvidersMap[T, TF, RF, TYP, DEC, PDEC] private (
    val members: Buffer[Dag[T]],
    val polyMembers: Buffer[TF],
    val topLevelRefsSet: MSet[RF],
    val decoratorsBuffer:Buffer[(TYP, DEC)],
    val polyDecoratorsBuffer:Buffer[PDEC]) {

  def getDecorators(pred:TYP => Boolean):Seq[DEC] = {
    decoratorsBuffer.collect { case (k,v) if pred(k) => v }.toSeq
  }
  
  def findMembers(typPredicate: T => Boolean): Seq[Dag[T]] =
    members.filter((dag) => typPredicate(dag.value))

  def findMembers1[R](select: Dag[T] => Option[R]): Seq[R] =
    members.map(select).collect { case Some(v) => v }
    
  def findPolymorphicMembers[R](select: TF => Option[R]): Seq[R] =
    polyMembers.map(select).collect { case Some(v) => v }
  
  def findPolymorphicDecorators[R](select: PDEC => Option[R]): Seq[R] =
    polyDecoratorsBuffer.map(select).collect { case Some(v) => v }

  def ++=(e: ProvidersMap[T, TF, RF, TYP, DEC, PDEC]): Unit = {
    members ++= e.members
    polyMembers ++= e.polyMembers 
    topLevelRefsSet ++= e.topLevelRefsSet
    decoratorsBuffer ++= e.decoratorsBuffer
    polyDecoratorsBuffer ++= e.polyDecoratorsBuffer
  }

  def topLevelRefs: Set[RF] = topLevelRefsSet.toSet
  def decorators: Seq[(TYP, DEC)] = decoratorsBuffer

  def copy() = new ProvidersMap(members.clone(), polyMembers.clone(), topLevelRefsSet.clone(), decoratorsBuffer.clone(), polyDecoratorsBuffer.clone())
}

