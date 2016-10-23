package com.github.gdefacci.di.macrodef

import collection.mutable.{Set => MSet}

private[di] object ProvidersMap {

  def empty[KI, T, TF, RF] = new ProvidersMap[KI, T, TF, RF](MapOfBuffers.empty, MapOfBuffers.empty, MSet.empty)

  def apply[KI, T, TF, RF](mp: Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)], topLevelRefs: Set[RF]): ProvidersMap[KI, T, TF, RF] = {
    val res = empty[KI, T, TF, RF]

    mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res += (k, _))
    }
    polyMembersMap.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res.poly += (k, _))
    }
    res.topLevelRefsSet ++= topLevelRefs
    res
  }

}

 

private[di] class ProvidersMap[KI, T, TF, RF] private (
    private val membersMap: MapOfBuffers[KI, Dag[T]],
    private val polyMembersMap: MapOfBuffers[KI, TF],
    private val topLevelRefsSet: MSet[RF]) {

  def topLevelRefs: Set[RF] = topLevelRefsSet.toSet
  
  def findMembers(kid: KI, typPredicate: T => Boolean): Seq[Dag[T]] =
    membersMap.find(kid, (dag) => typPredicate(dag.value))

  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] =
    polyMembersMap.values(kid).map(select).collect { case Some(v) => v }

  def +=(e: (KI, Dag[T])): Unit = membersMap += (e._1, e._2)
  def ++=(mp: Seq[(KI, Dag[T])]): Unit = membersMap ++= mp

  object poly {
    def +=(e: (KI, TF)): Unit = polyMembersMap += (e._1, e._2)
    def ++=(mp: Seq[(KI, TF)]): Unit = polyMembersMap ++= mp
  }

  def ++=(e: ProvidersMap[KI, T, TF, RF]): Unit = {
    membersMap ++= e.membersMap
    polyMembersMap ++= e.polyMembersMap
    topLevelRefsSet ++= e.topLevelRefsSet
  }

  def members: Seq[Dag[T]] = membersMap.values
  def polyMembers: Seq[TF] = polyMembersMap.values

  def copy() = new ProvidersMap(membersMap.copy(), polyMembersMap.copy(), topLevelRefsSet.clone())
}

