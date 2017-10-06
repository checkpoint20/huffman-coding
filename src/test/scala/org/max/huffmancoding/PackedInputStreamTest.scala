package org.max.huffmancoding

import java.io.ByteArrayInputStream

class PackedInputStreamTest extends org.scalatest.FlatSpec {

  "PackedInputStream" should "return stream of booleans" in {

    val input = Array[Byte](
      -128, // 10000000
      -116, // 10001100
      -1,   // 11111111
      1     // 00000001
    )

    val expected = Stream(
      true,  false, false, false, false, false, false, false,
      true,  false, false, false, true,  true,  false, false,
      true,  true,  true,  true,  true,  true,  true,  true,
      false, false, false, false, false, false, false, true)

    val provided = new PackedInputStream(new ByteArrayInputStream(input))
    assert(expected equals provided.stream())
  }

}
