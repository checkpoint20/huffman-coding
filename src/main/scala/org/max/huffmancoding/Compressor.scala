package org.max.huffmancoding

import java.io._

import org.max.huffmancoding.HuffmanTree.{buildDictionary, buildHeap, buildTree, serialize}

import scala.io.Source

object Compressor {

  def main(args: Array[String]): Unit = {
    val freqs = collectFrequencies(args(0))
    val is = new FileInputStream(args(0))
    val os = new FileOutputStream(args(1))
    try {
      compress(freqs, is, os)
    } finally {
      is.close()
      os.close()
    }
  }

  def compress(freqs: Set[Leaf], is: InputStream, os: OutputStream): Map[Char, Code] = {
    val dict: Map[Char, Code] = buildDictionary(buildTree(buildHeap(freqs)))
    val reader = Source.fromInputStream(is).bufferedReader()
    val pos = new PackedOutputStream(os)
    try {
      os.write(serialize(freqs))
      os.flush()
      pos.write(streamOfReader(reader).map(char => dict(char)))
    } finally {
      reader.close()
      pos.close()
    }
    dict
  }

  private def collectFrequencies(inputFile: String): Set[Leaf] = {
    val reader = new BufferedReader(new FileReader(inputFile))
    try {
      HuffmanTree.collectFrequencies(streamOfReader(reader))
    } finally {
      reader.close()
    }
  }

  private def streamOfReader(reader: Reader): Stream[Char] =
    Stream.continually(reader.read()).takeWhile(_ != -1).map(_.toChar)

}
