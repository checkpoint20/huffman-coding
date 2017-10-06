package org.max.huffmancoding

import java.io._

import scala.annotation.tailrec
import scala.collection.mutable

object HuffmanTree {

  val EOF = '\u0004'

  /**
    * Collects frequencies of chars in the input set. Always add EOF (u0004)
    * character with 0 frequency to the output.
    *
    * @param input set of chars.
    * @return A set contains unique weighted chars.
    */
  def collectFrequencies(input: Traversable[Char]): Set[Leaf] =
    input.groupBy(identity).mapValues(t => t.count(_ => true))
      .map(p => Leaf(p._1, p._2)).toSet + Leaf(EOF, 0)

  def buildHeap(coll: TraversableOnce[Node]): mutable.PriorityQueue[Node] =
    mutable.PriorityQueue[Node]()(Ordering.by(- _.weight)) ++ coll

  @tailrec def buildTree(heap: mutable.PriorityQueue[Node]): InternalNode =
    if (heap.size == 1) heap.dequeue().asInstanceOf[InternalNode]
    else buildTree(heap += InternalNode(heap.dequeue(), heap.dequeue()))

  def buildDictionary(root: InternalNode): Map[Char, Code] = {
    @tailrec def dfs(queue: List[(Node, Code)], dict: Map[Char, Code]): Map[Char, Code] = {
      queue match {
        case Nil => dict
        case (node, code) :: rest => node match {
          case Leaf(c, _) => dfs(rest, dict + (c -> code))
          case InternalNode(l, r) => dfs((l, code.goLeft) :: (r, code.goRight) :: rest, dict)
        }
      }
    }

    dfs((root, Code(0, 0)) :: Nil, Map())
  }

  /**
    * Serialize a set of char weights as a byte array:
    * <ol>
    * <li>First 4 bytes - an int representing N - the length of the set
    * <li>Following by - N-pairs of {2 bytes char, 4 bytes int weight of the char}
    * </ol>
    * @param chars a set of chars with weights to serialize.
    * @return Byte array.
    */
  def serialize(chars: Set[Leaf]): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(bos)
    val size = Leaf.Bytes * chars.size
    dos.writeInt(size)
    chars.foreach(leaf => {
      dos.writeChar(leaf.c)
      dos.writeInt(leaf.weight)
    })
    bos.toByteArray
  }

  /**
    * Deserialize a ser of chars with weights from an input stream:
    * <ol>
    * <li>Read length of the set from the first 4 bytes
    * <li>Following by - N-pairs of {2 bytes char, 4 bytes int weight of the char}
    * </ol>
    * @param is input stream to read.
    * @return Set of chars with their weights.
    */
  def deserialize(is: InputStream): Set[Leaf] = {
    val dis = new DataInputStream(is)
    val size = dis.readInt() / Leaf.Bytes
    @tailrec def readDataStream(i: Int, set: Set[Leaf]): Set[Leaf] = {
      if(i >= size) set
      else readDataStream(i + 1, set + Leaf(dis.readChar(), dis.readInt()))
    }
    readDataStream(0, Set())
  }
}


class Node(val weight: Int)

case class Leaf(c: Char, override val weight: Int) extends Node(weight)

object Leaf {
  val Bytes: Int = Character.BYTES + Integer.BYTES
}

case class InternalNode(l: Node, r: Node) extends Node(l.weight + r.weight) {
  def child(dir: Boolean): Node = if (dir) r else l
}

class TreeNavigator(root: InternalNode) {
  private var currNode: InternalNode = root

  def next(dir: Boolean): Node = {
    val child: Node = currNode.child(dir)
    currNode = child match {
      case node: InternalNode => node
      case _ => root
    }
    child
  }
}