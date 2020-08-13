package tserver

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousSocketChannel

import scodec.Codec
import scodec.bits.{Bases, BitVector, ByteVector}
import scodec.codecs.implicits.{implicitStringCodec => _, _}

object Nio2Client {

  def main(args: Array[String]): Unit = {

   val readomN = ByteVector.fromValidHex("0x3E0549828CCA27E966B301A48FECE2FC",Bases.Alphabets.HexUppercase)
   val arr = ReqPQ(0,5901257869632771658L,20,1615239032,readomN)

    val arrBinary = Codec.encode(arr).require.toByteVector
    println(arrBinary)

    val client = AsynchronousSocketChannel.open
    val hostAddress = new InetSocketAddress("localhost", 8080)
    val future = client.connect(hostAddress)

    future.get

    val byteMsg1 = new String("bla1").getBytes
    val byteMsg2 = new String("cat2").getBytes
    val buffer1 = ByteBuffer.wrap(byteMsg1)
    val buffer2 = ByteBuffer.wrap(byteMsg2)
    val bufferRead = ByteBuffer.allocate(256)


    val writeResult1 = client.write(ByteBuffer.wrap(arrBinary.toArray))
    val s1 = writeResult1.get
    println("Status1: " + s1)

   val readResult = client.read(bufferRead)
   val rs = readResult.get
   val resArray = bufferRead.array()
   val vector = ByteVector.apply(resArray)
   println("Read status: " + rs)
   println("Read vector: " + vector)
   println("Read message: " + ResPQCodec.decode(vector.toBitVector).require.value)

   //val writeResult2 = client.write(buffer2)
    //val s2 = writeResult2.get
    //println("Status2: " + s2)



    val echo = new String(bufferRead.array).trim

    println("Data: " + echo)

  }

}
