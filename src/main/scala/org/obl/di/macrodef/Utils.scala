package org.obl.di.macrodef

private[di] object Utils {

  def byLengthsPartition[T](lengths: Seq[Int], arguments: List[T]): List[List[T]] = {
    assert(lengths.sum == arguments.length, s"wrong number of arguments")
    lengths match {
      case Nil => Nil
      case hd :: Nil =>
        assert(hd == arguments.length)
        List(arguments)
      case hd +: rest =>
        val (curr, restArgs) = arguments.splitAt(hd)
        curr +: byLengthsPartition(rest, restArgs)
    }
  }
  
}
