package org.max.huffmancoding

import java.io.InputStream

class PackedInputStream (val is: InputStream) extends AutoCloseable {

  def stream(): Stream[Boolean] =
    Stream.continually(is.read()).takeWhile(_ != -1).flatMap(
      i => 0 to 7 map(o => ((PackedInputStream.One >>> o) & i) != 0)
    )

  override def close(): Unit = is.close()
}

object PackedInputStream {
  val One: Int = Integer.parseUnsignedInt("00000000000000000000000010000000", 2)
}