package org.max.huffmancoding

import java.io._

import org.max.huffmancoding.HuffmanTree.{buildHeap, buildTree, deserialize}

object Decompressor {

  def main(args: Array[String]): Unit = {
    val is = new FileInputStream(args(0))
    val os = new FileOutputStream(args(1))
    try {
      decompress(is, os)
    } finally {
      is.close()
      os.close()
    }
  }

  def decompress(is: InputStream, os: OutputStream): Unit = {
    val pis = new PackedInputStream(is)
    val writer = new OutputStreamWriter(os)
    try {
      val dict = buildTree(buildHeap(deserialize(is)))
      val navigator = new TreeNavigator(dict)
      pis.stream()
        .map(bit => navigator.next(bit))
        .filter(_.isInstanceOf[Leaf])
        .map(_.asInstanceOf[Leaf])
        .takeWhile(_.c != HuffmanTree.EOF)
        .foreach(leaf => writer.write(leaf.c))
    } finally {
      pis.close()
      writer.close()
    }
  }

}
