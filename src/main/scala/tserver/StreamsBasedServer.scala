package tserver


import java.nio.ByteBuffer

import cats.implicits.toTraverseOps
import scodec.bits.ByteVector
import zio.console.Console
import zio.{RIO, _}
import zio.nio.channels._
import zio.nio.core.SocketAddress
import zio.stream._
import zio.interop.catz.{console => _, _}

import scala.language.implicitConversions

object StreamsBasedServer extends App {

  def run(args: List[String]): URIO[Console, ExitCode] =
    server(8080)
      .use(handleConnections(_, processor,pDecoder,pEncoder))
      .exitCode



  def server(port: Int): Managed[Exception, AsynchronousServerSocketChannel] =
    for {
      server        <- AsynchronousServerSocketChannel()
      socketAddress <- SocketAddress.inetSocketAddress(port).toManaged_
      _             <- server.bind(socketAddress).toManaged_
    } yield server

  def handleConnections[R <: Console](
                                     server: AsynchronousServerSocketChannel,
                                     processor: Protocol => RIO[R, List[Protocol]],
                                     protocolDecoder: Array[Byte] => RIO[R,Protocol],
                                     protocolEncoder: Protocol => RIO[R,Array[Byte]]): RIO[R, Unit] =
    ZStream
      .repeat(server.accept)
      .flatMap(conn => ZStream.managed(conn.ensuring(console.putStrLn("Connection closed"))))
      .mapM { channel =>
            val r = for {
              _    <- console.putStrLn("Received connection")
              data <- channel.read(256)
              protocolObject <- protocolDecoder(data.toArray)
              respObjects    <- processor(protocolObject)
              respEncoded    <- respObjects.map(pEncoder).sequence
              writtenBytes <- respEncoded.map(arr => channel.write(Chunk.fromArray(arr))).sequence
              _    <- console.putStrLn("Bytes written: "+writtenBytes)
            } yield ()
            r.catchAllDefect(e=>URIO(e.printStackTrace()))
             .catchAll(e=>URIO(e.printStackTrace()))
      }.runDrain

  def processor[R <: Console](proto: Protocol):RIO[R, List[Protocol]] = {
      console.putStrLn(s"Read data: ${proto.toString}") *>
      console.putStrLn("Done") *>
      RIO(List[Protocol](ResPQ(1,3,4,5,ByteVector.fromInt(5))))
  }

  def pEncoder[R <: Console](arr: Protocol):RIO[R, Array[Byte]] = arr match {
    case res: ResPQ =>
      val vec = ResPQCodec.encode(res).require
     console.putStrLn(s"Write data: ${vec}") *>
      ZIO(vec.toByteArray)
    case e => ZIO.fail(new Exception("Encoding not supported for:"+e))
  }


  def pDecoder[R](arr: Array[Byte]):RIO[R, Protocol]  = {
    println("Hex: "+ByteVector.apply(arr))
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