package org.obl.di.macrodef

object MProvidersMap {

  def empty[KI, T, TF] = new MProvidersMap[KI, T, TF]

  def apply[KI, T, TF](mp: Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)]): MProvidersMap[KI, T, TF] = {
    val res = empty[KI, T, TF]

    mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res += (k, _))
    }
    polyMembersMap.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res.poly += (k, _))
    }
    res
  }

}
private[di] class MProvidersMap[KI, T, TF] private ( 
    private val membersMap:MapOfBuffers[KI, Dag[T]],
    private val polyMembersMap:MapOfBuffers[KI, TF]) {
  
  def this() = this(new MapOfBuffers[KI, Dag[T]], new MapOfBuffers[KI, TF])
  
  def findMembers(kid: KI, typPredicate: T => Boolean): Seq[Dag[T]] = 
    membersMap.find(kid, (dag) => typPredicate(dag.value))
  
  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] = 
    polyMembersMap.values(kid).map(select).collect { case Some(v) => v }
  
  def +=(e: (KI, Dag[T])): Unit = membersMap += (e._1, e._2)
  def ++=(mp: Seq[(KI, Dag[T])]): Unit = membersMap ++= (mp)
    
  object poly {
    def +=(e: (KI, TF)): Unit = polyMembersMap += (e._1, e._2)
    def ++=(mp: Seq[(KI, TF)]): Unit = polyMembersMap ++= mp
  }
  
  def ++=(e: MProvidersMap[KI, T, TF]): Unit = {
    membersMap ++= e.membersMap
    polyMembersMap ++= e.polyMembersMap
  }
  
  //def replaceMember(id:KI, what:Dag[T] => Boolean, withDag:Dag[T]) = membersMap.replace(id, what, withDag)
  
  def members: Seq[Dag[T]] = membersMap.values
  def polyMembers: Seq[TF] = polyMembersMap.values
    
  def copy() = new MProvidersMap(membersMap.copy(), polyMembersMap.copy())
}

