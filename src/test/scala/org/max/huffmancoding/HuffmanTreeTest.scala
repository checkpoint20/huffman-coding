package org.max.huffmancoding

import java.io.ByteArrayInputStream

import org.scalatest.FlatSpec

class HuffmanTreeTest extends FlatSpec {

  "collectFrequencies" should "build a map of chars to frequencies" in {
    val expected = Set(
      Leaf('H', 1),
      Leaf('e', 1),
      Leaf('l', 3),
      Leaf('o', 2),
      Leaf(' ', 1),
      Leaf('w', 1),
      Leaf('r', 1),
      Leaf('d', 1),
      Leaf('!', 1),
      Leaf(HuffmanTree.EOF, 0)
    )
    val actual = HuffmanTree.collectFrequencies("Hello world!")
    assert(actual == expected)
  }

  "buildHeap" should "build a heap that guaranties dequeue of a min element" in {
    val provided = Array(new Node(3), new Node(1), new Node(5), new Node(2), new Node(4))
    val actual = HuffmanTree.buildHeap(provided)

    assert(actual.dequeue().weight == 1)
    assert(actual.dequeue().weight == 2)
    assert(actual.dequeue().weight == 3)
    assert(actual.dequeue().weight == 4)
    assert(actual.dequeue().weight == 5)
    assert(actual.isEmpty)

  }

  "buildTree" should "build a Huffman tree" in {
    val provided = Array(
      Leaf('a', 5),
      Leaf('b', 9),
      Leaf('c', 12),
      Leaf('d', 13),
      Leaf('e', 16),
      Leaf('f', 45)
    )

    val actual = HuffmanTree.buildTree(HuffmanTree.buildHeap(provided))

    assert(actual.weight == 100)
    assert(actual.isInstanceOf[InternalNode])
    assert(actual.asInstanceOf[InternalNode].l == Leaf('f', 45))
    assert(actual.asInstanceOf[InternalNode].r.weight == 55)
  }

  "buildDictionary" should "build a map Char -> Code" in {
    val provided = Array(
      Leaf('a', 5),
      Leaf('b', 9),
      Leaf('c', 12),
      Leaf('d', 13),
      Leaf('e', 16),
      Leaf('f', 45)
    )

    val treeRoot = HuffmanTree.buildTree(HuffmanTree.buildHeap(provided))
    val actual = HuffmanTree.buildDictionary(treeRoot)
    val expected = Map (
      'f' -> Code(Integer.parseUnsignedInt("00000000000000000000000000000000", 2), 1),
      'c' -> Code(Integer.parseUnsignedInt("10000000000000000000000000000000", 2), 3),
      'd' -> Code(Integer.parseUnsignedInt("10100000000000000000000000000000", 2), 3),
      'a' -> Code(Integer.parseUnsignedInt("11000000000000000000000000000000", 2), 4),
      'b' -> Code(Integer.parseUnsignedInt("11010000000000000000000000000000", 2), 4),
      'e' -> Code(Integer.parseUnsignedInt("11100000000000000000000000000000", 2), 3)
    )

    assert(actual == expected)
  }

  "serialize" should "save a set of chars with weights to a byte buffer" in {
    val provided = Set(
      Leaf('a', 5),
      Leaf('b', 9)
    )

    val actual = HuffmanTree.serialize(provided)
    assert(actual.length == (Integer.BYTES + (Leaf.Bytes * 2)))
  }

  "deserialize" should "read a set of chars with weights from a byte buffer" in {
    val expected = Set(
      Leaf('a', 5),
      Leaf('b', 9)
    )

    val array = HuffmanTree.serialize(expected)
    val actual = HuffmanTree.deserialize(new ByteArrayInputStream(array))
    assert(actual == expected)
  }

  "TreeNavigator" should "go through a tree in the specified direction" in {
    val d = Leaf('f', 13)
    val e = Leaf('e', 16)
    val f = Leaf('f', 45)
    val ed = InternalNode(e, d)
    val root = InternalNode(f, ed)

    val navigator = new TreeNavigator(root)
    assert(navigator.next(false) == f)
    assert(navigator.next(true)  == ed)
    assert(navigator.next(false) == e)
    assert(navigator.next(true)  == ed)
    assert(navigator.next(true)  == d)
  }
}
