package com.github.gdefacci.di.macrodef

import collection.mutable.{Set => MSet, Buffer}

private[di] object ProvidersMap {

  def empty[KI, T, TF, RF, TYP, DEC] = new ProvidersMap[KI, T, TF, RF, TYP, DEC](MapOfBuffers.empty, MapOfBuffers.empty, MSet.empty, Buffer.empty)
  
  def apply[KI, T, TF, RF, TYP, DEC](mp: Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)], topLevelRefs: Set[RF], decoratorsMap:Seq[(TYP, DEC)]): ProvidersMap[KI, T, TF, RF, TYP, DEC] = {
    val res = empty[KI, T, TF, RF, TYP, DEC]
    res.decoratorsBuffer ++= decoratorsMap
    
    mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res.membersMap += (k, _))
    }
    polyMembersMap.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res.polyMembersMap += (k, _))
    }
    res.topLevelRefsSet ++= topLevelRefs
    res
  }
}

private[di] class ProvidersMap[KI, T, TF, RF, TYP, DEC] private (
    val membersMap: MapOfBuffers[KI, Dag[T]],
    val polyMembersMap: MapOfBuffers[KI, TF],
    val topLevelRefsSet: MSet[RF],
    val decoratorsBuffer:Buffer[(TYP, DEC)]) {

  def getDecorators(pred:TYP => Boolean):Seq[DEC] = {
    decoratorsBuffer.collect { case (k,v) if pred(k) => v }.toSeq
  }
  
  def findMembers(kid: KI, typPredicate: T => Boolean): Seq[Dag[T]] =
    membersMap.find(kid, (dag) => typPredicate(dag.value))

  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] =
    polyMembersMap.values(kid).map(select).collect { case Some(v) => v }

  def ++=(e: ProvidersMap[KI, T, TF, RF, TYP, DEC]): Unit = {
    membersMap ++= e.membersMap
    polyMembersMap ++= e.polyMembersMap
    topLevelRefsSet ++= e.topLevelRefsSet
    decoratorsBuffer ++= e.decoratorsBuffer;
  }

  def topLevelRefs: Set[RF] = topLevelRefsSet.toSet
  def members: Seq[Dag[T]] = membersMap.values
  def polyMembers: Seq[TF] = polyMembersMap.values
  def decorators: Seq[(TYP, DEC)] = decoratorsBuffer

  def copy() = new ProvidersMap(membersMap.copy(), polyMembersMap.copy(), topLevelRefsSet.clone(), decoratorsBuffer.clone())
}

