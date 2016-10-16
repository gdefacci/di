package com.github.gdefacci.di.tests

import org.scalatest.FunSuite
import com.github.gdefacci.di.macrodef.GenModel

class GenModelTest extends FunSuite {

  test("partition") {

    val gen = new GenModel[Int, String, String]

    val bl = new gen.Block(
      10,
      new gen.Declaration("b", new gen.Block(
        11,
        new gen.Declaration("ba", new gen.Value(2, "bbaa")) ::
          Nil,
        "block1")) ::
      new gen.Declaration("a", new gen.Value(4, "aa")) ::
      new gen.Declaration("c", new gen.Block(
        12,
        new gen.Declaration("caba", new gen.Value(123, "caba")) ::
          Nil,
        "block2")) ::
      Nil,
      "")
    
    val (bl1, decls) = gen.partitionContent(_.source % 2 == 0)(bl)
    
    assert(gen.allDeclarations(bl1 :: Nil, _.source).forall(_.expression.source % 2 == 0))

    assert(decls.forall(_.expression.source % 2 != 0))

  }

}