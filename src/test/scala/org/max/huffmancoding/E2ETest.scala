package org.max.huffmancoding

import java.io._

import org.scalatest.FlatSpec

class E2ETest extends FlatSpec {

  "Compress/Decompress cycle" should "produce output the same as input" in {
    val input = "Hello world!"
    val freqs = HuffmanTree.collectFrequencies(input)
    println("Char frequencies:\n" + freqs.toList.sortWith(_.weight > _.weight).mkString("\n"))

    val compressedOs = new ByteArrayOutputStream()
    val dict = Compressor.compress(
      freqs,
      new ByteArrayInputStream(input.getBytes),
      compressedOs
    )
    println("Dictionary:\n" + dict.toList.sortWith(_._2.length < _._2.length).mkString("\n"))

    val compressed = compressedOs.toByteArray

    val decompressedOs = new ByteArrayOutputStream()
    Decompressor.decompress(new ByteArrayInputStream(compressed), decompressedOs)

    val output: String =
      new BufferedReader(
        new InputStreamReader(
          new ByteArrayInputStream(decompressedOs.toByteArray)
        )
      ).readLine()

    println(output)

    assert(output == input)
  }


}
