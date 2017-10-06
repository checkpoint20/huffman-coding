package org.max.huffmancoding

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream}

import org.scalatest.FlatSpec

class PackedOutputStreamTest extends FlatSpec {

  "When the input stream of 1 element less that size" should "output this element" in {
    val in1 =       Code(Integer.parseUnsignedInt("01010000000000000000000000000000", 2), 5)
    val expected1 = Code(Integer.parseUnsignedInt("01010000000000000000000000000000", 2), 5)

    val input = Stream(in1)
    val expected = Array(expected1)

    val os = new ByteArrayOutputStream()
    new PackedOutputStream(os).write(input)
    os.flush()
    val dis = new DataInputStream(new ByteArrayInputStream(os.toByteArray))

    var actual: Array[Int] = Array()
    while (dis.available() > 0) {
      actual = actual :+ dis.readInt()
    }

    actual.map(Code.toBinaryWithLeadingZeroes).foreach(println(_))

    assert(expected.map(_.code) sameElements actual)

  }

  "The stream" should "pack input stream" in {
    val in1 =       Code(Integer.parseUnsignedInt("01010000000000000000000000000000", 2), 5)
    val in2 =       Code(Integer.parseUnsignedInt("11111111111111111000000000000000", 2), 17)
    val in3 =       Code(Integer.parseUnsignedInt("01010101010101010101010101010101", 2), 32)

    val expected1 = Code(Integer.parseUnsignedInt("01010111111111111111110101010101", 2), 32)
    val expected2 = Code(Integer.parseUnsignedInt("01010101010101010101010000000000", 2), 23)

    val provided = Stream(in1, in2, in3)
    val expected = Array(expected1, expected2)

    val os = new ByteArrayOutputStream()
    new PackedOutputStream(os).write(provided)
    os.flush()
    val dis = new DataInputStream(new ByteArrayInputStream(os.toByteArray))

    var actual: Array[Int] = Array()
    while (dis.available() > 0) {
      actual = actual :+ dis.readInt()
    }

    actual.map(Code.toBinaryWithLeadingZeroes).foreach(println(_))

    assert(expected.map(_.code) sameElements actual)
  }

}
