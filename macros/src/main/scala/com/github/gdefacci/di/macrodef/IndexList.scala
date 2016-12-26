package com.github.gdefacci.di.macrodef

class IndexStack[T](var elements:List[T] = Nil) {
  
  def map[B](f:T => B):List[B] = elements.map(f)
  
  def pushIfNotDuplicated(itm:T, onDuplicate: () => Unit):Unit = {
    if (elements.contains(itm)) {
      onDuplicate()
    } else {
      elements = itm :: elements
    }
  }
  
  def pop():Unit = {
    elements match {
      case Nil => throw new NoSuchElementException
      case hd :: newElements => 
        elements = newElements
    }
  }
  
  def copy() = {
    new IndexStack[T](elements)
  }
}