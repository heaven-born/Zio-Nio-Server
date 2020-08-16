package tserver.common

import cats.implicits.toTraverseOps
import scodec.bits.ByteVector
import tserver.common.ZioNioTcpServer.Result
import zio.{Chunk, Managed, Queue, RIO, Semaphore, URIO, ZIO, console}
import zio.console.Console
import zio.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel}
import zio.nio.core.SocketAddress
import zio.stream.ZStream
import zio.interop.catz.{console => _, _}

/**
 * Purely functional lightweight general purpose TCP server based on ZIO and NIO2.
 *
 * Method "run" accepts:
 *  decoder - converts byte array from requests to type T (usually sealed trait)
 *  encoder - converts type T back to byte array
 *  processor - contains business logic accepting type T (request) and returning list of T (responses)
 *
 * @param port
 * @param numOfParallelRequests - num of requests that can be served at the ame time.
 */
class ZioNioTcpServer(port: Int, numOfParallelRequests:Int = 5) {

  def run[R <: Console,T]( processor: T => RIO[R, Result[List[T]]],
                           encoder: T => RIO[R,Array[Byte]],
                           decoder: Array[Byte] => RIO[R,T]): RIO[R, Unit] =
    server(port)
      .use(handleConnections(_, processor,decoder,encoder))



  private def server(port: Int): Managed[Exception, AsynchronousServerSocketChannel] =
    for {
      server        <- AsynchronousServerSocketChannel()
      socketAddress <- SocketAddress.inetSocketAddress(port).toManaged_
      _             <- server.bind(socketAddress).toManaged_
    } yield server


  private def handleConnections[R<: Console,T]( server: AsynchronousServerSocketChannel,
                                 processor: T => RIO[R, Result[List[T]]],
                                 protocolDecoder: Array[Byte] => RIO[R,T],
                                 protocolEncoder: T => RIO[R,Array[Byte]]): RIO[R, Unit] = {

    def processConnection(channel: AsynchronousSocketChannel) = {

      def r: RIO[R, Unit] = for {
        requestData <- channel.read(1024)
        _    <- console.putStrLn("Data received: "+ ByteVector(requestData))
        requestObject <- protocolDecoder(requestData.toArray)
        _    <- console.putStrLn("Object received: "+requestObject)
        respResult    <- processor(requestObject)
        _    <- console.putStrLn(s"[keepConnection=${respResult.keepConnection}]")
        _    <- console.putStrLn(s"Objects to be sent: ${respResult.res}")
        respData    <- respResult.res.map(protocolEncoder).sequence
        _    <- console.putStrLn("Data to be sent: "+respData.map(ByteVector(_)))
        writtenBytes <- respData.map(arr => channel.write(Chunk.fromArray(arr))).sequence
        _    <- console.putStrLn("Bytes sent: "+writtenBytes)
        // next line allows perform multiple reads during one session
        _ <- if (respResult.keepConnection) r else ZIO.unit
      } yield ()

      r.catchAllDefect(e=>URIO(e.printStackTrace())).catchAll(e=>URIO(e.printStackTrace()))
    }

    for {
      acceptBarrier <- Queue.bounded[Unit](1) // <- ugly trick to support parallel requests
      _ <- acceptBarrier.offer(())
      _ <- ZStream
          .repeatEffect{acceptBarrier.take *> ZIO(server.accept)}
          .mapMPar(numOfParallelRequests){ _.use { channel =>
              console.putStrLn("Received connection") *>
              acceptBarrier.offer() *>
              processConnection(channel)}
          }.runDrain
    } yield ()


  }

}


object ZioNioTcpServer {
  case class Result[T](res:T,keepConnection: Boolean = false)
}
