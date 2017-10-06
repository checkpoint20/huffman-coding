package org.max.huffmancoding

import java.io.OutputStream

import scala.annotation.tailrec

class PackedOutputStream(val os: OutputStream) extends AutoCloseable {

  def write(is: Stream[Code]): Unit = writeRec(is, Code(0, 0))

  @tailrec final def writeRec(is: Stream[Code], byteOut: Code): Unit = {
    if(is.isEmpty) os.write(byteOut.toByteArray)
    else {
      val byteIn = is.head
      if (byteOut.capacity == 0) {
        os.write(byteOut.toByteArray)
        writeRec(is, Code(0, 0))
      } else if (byteOut.capacity >= byteIn.length)
        writeRec(is.tail, byteOut.merge(byteIn))
      else writeRec(
        byteIn.getStart(byteOut.capacity) #:: byteIn.getRest(byteOut.capacity) #:: is.tail,
        byteOut)
    }
  }

  override def close(): Unit = os.close()
}
