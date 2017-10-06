package org.max.huffmancoding


case class Code(code: Int, length: Int) {

  def capacity: Int = Code.Size - length

  def merge(that: Code): Code = Code (
      code | (that.code >>> length),
      if(that.length + length > Code.Size) Code.Size else that.length + length
    )

  def getRest(offset: Int): Code = Code(code << offset, length - offset)

  def getStart(offset: Int): Code = Code(code, offset)

  def toByteArray: Array[Byte] = Array(
    (code >>> 24).toByte,
    (code >>> 16).toByte,
    (code >>> 8).toByte,
    code.toByte)

  def goRight: Code = Code(Code.setNthBitToOne(code, length), length + 1)

  def goLeft: Code = Code(Code.setNthBitToZero(code, length), length + 1)

  override def toString: String = s"Code(${Code.toBinaryWithLeadingZeroes(code)}, $length)"

}

object Code {
  val Size: Int =  Integer.SIZE
  val One: Int  = Integer.parseUnsignedInt("10000000000000000000000000000000", 2)

  def setNthBitToZero(v: Int, n: Int): Int = ~(One >>> n) & v

  def setNthBitToOne(v: Int, n: Int): Int = (One >>> n) | v

  def toBinaryWithLeadingZeroes(int: Int): String = {
    val buf: StringBuilder = new StringBuilder(int.toBinaryString)
    while(buf.size < Size) {
      buf.insert(0, '0')
    }
    buf.toString()
  }
}
