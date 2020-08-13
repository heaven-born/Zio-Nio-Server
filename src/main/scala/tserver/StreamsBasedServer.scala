package tserver


import java.nio.ByteBuffer
import scodec.bits.ByteVector
import tserver.common.ZioNioTcpServer
import zio.console.Console
import zio.{RIO, _}
import zio.interop.catz.{console => _, _}

import scala.language.implicitConversions

object StreamsBasedServer extends App {

  type Env = Console

  def run(args: List[String]): URIO[Env, ExitCode] =
    new ZioNioTcpServer[Env,Protocol]("localhost",8080)
      .run(processor,encoder,decoder)
      .exitCode

  def processor(proto: Protocol):RIO[Env, List[Protocol]] = {
      RIO(List[Protocol](ResPQ(1,3,4,5,ByteVector.fromInt(5))))
  }

  def encoder(arr: Protocol):RIO[Env, Array[Byte]] = arr match {
    case res: ResPQ =>
      val vec = ResPQCodec.encode(res).require
     console.putStrLn(s"Response Hex: ${vec.toByteVector}") *>
      ZIO(vec.toByteArray)
    case e => ZIO.fail(new Exception("Encoding not supported for:"+e))
  }


  def decoder(arr: Array[Byte]):RIO[Env, Protocol]  = {
    println("Request Hex: "+ByteVector.apply(arr))
    val buffer = arr.slice(20,24)
    val constructor = ByteBuffer.wrap(buffer.reverse).getInt
    println("TL constructor (decimal): "+ constructor)
    constructor match {
      case 1615239032 => //reqPQ
        ZIO(reqpqcodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case e => ZIO.fail(new Exception("error decoding Array :"+arr.toList))
    }

  }

}