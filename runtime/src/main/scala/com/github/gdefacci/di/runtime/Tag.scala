package com.github.gdefacci.di.runtime

case class Tag[+V, +K](value: V)

object Tag {
  class WithTag[K] {
    def apply[V](v: V) = Tag[V, K](v)
  }

  def apply[K] = new WithTag[K]
}
  