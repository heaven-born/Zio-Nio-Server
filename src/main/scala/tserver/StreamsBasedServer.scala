package tserver


import java.nio.ByteBuffer

import scodec.bits.{Bases, ByteVector}
import tserver.common.ZioNioTcpServer
import zio.console.Console
import zio.{RIO, _}
import zio.interop.catz.{console => _, _}

import scala.language.implicitConversions

object StreamsBasedServer extends App {

  type Env = Console

  def run(args: List[String]): URIO[Env, ExitCode] =
    new ZioNioTcpServer("localhost",8080)
      .run(processor,encoder,decoder)
      .exitCode

  def processor(proto: Protocol):RIO[Env, List[Protocol]] = {
    val nonce = ByteVector.fromValidHex("0x3E0549828CCA27E966B301A48FECE2FC",Bases.Alphabets.HexUppercase)
    val server_nonce = ByteVector.fromValidHex("0xA5CF4D33F4A11EA877BA4AA573907330",Bases.Alphabets.HexUppercase)
    val pq = BigInt("1724114033281923457")
    val fongerprint = BigInt("2408273976350192835")
    val resp = ResPQ(0,5901257890957871105L,64,res_pq_constructor,nonce,server_nonce,pq,vector_long_con,1,fongerprint)
    RIO(List[Protocol](resp))
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
    println("TL constructor (HEX): "+ ByteVector.apply(buffer))
    val constructor = ByteBuffer.wrap(buffer.padTo(8,0.toByte).reverse).getLong
    println("TL constructor (decimal): "+ constructor)
    constructor match {
      case `req_pq_constructor` =>
        ZIO(ReqPQCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case `req_dh_params_constructor` =>
        ZIO(ReqDHParamsCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case e => ZIO.fail(new Exception("error decoding Array :"+arr.toList))
    }

  }

}