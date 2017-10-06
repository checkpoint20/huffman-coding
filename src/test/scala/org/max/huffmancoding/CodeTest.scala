package org.max.huffmancoding

import org.scalatest.FlatSpec

class CodeTest extends FlatSpec {

  "Code merge" should "put two codes inline" in {
    val code =     Code(Integer.parseUnsignedInt("10000000000000000000000000000000", 2), 1)
    val that =     Code(Integer.parseUnsignedInt("11000000000000000000000000000000", 2), 2)
    val expected = Code(Integer.parseUnsignedInt("11100000000000000000000000000000", 2), 3)
    val actual =   code.merge(that)
    println(actual)
    assert(expected.equals(actual))
  }

  "Code getRest" should "extract a code after an offset" in {
    val code =     Code(Integer.parseUnsignedInt("10110001000000000000000000000000", 2), 8)
    val expected = Code(Integer.parseUnsignedInt("10001000000000000000000000000000", 2), 5)
    val actual =   code.getRest(3)
    println(actual)
    assert(expected.equals(actual))
  }

  "Code getStart" should "extract a code before an offset including" in {
    val code =     Code(Integer.parseUnsignedInt("10110001000000000000000000000000", 2), 8)
    val expected = Code(Integer.parseUnsignedInt("10110001000000000000000000000000", 2), 3)
    val actual =   code.getStart(3)
    println(actual)
    assert(expected.equals(actual))
  }

  "Code goRight" should "set next bit to 1" in {
    val code =     Code(Integer.parseUnsignedInt("10110001000000000000000000000000", 2), 8)
    val expected = Code(Integer.parseUnsignedInt("10110001100000000000000000000000", 2), 9)
    val actual =   code.goRight
    println(actual)
    assert(expected.equals(actual))
  }

  "Code goLeft" should "set next bit to 0" in {
    val code =     Code(Integer.parseUnsignedInt("10110001100000000000000000000000", 2), 8)
    val expected = Code(Integer.parseUnsignedInt("10110001000000000000000000000000", 2), 9)
    val actual =   code.goLeft
    println(actual)
    assert(expected.equals(actual))
  }

}
