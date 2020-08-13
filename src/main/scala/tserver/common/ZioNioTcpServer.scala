package tserver.common

import cats.implicits.toTraverseOps
import zio.{Chunk, Managed, RIO, URIO, console}
import zio.console.Console
import zio.nio.channels.AsynchronousServerSocketChannel
import zio.nio.core.SocketAddress
import zio.stream.ZStream
import zio.interop.catz.{console => _, _}

class ZioNioTcpServer(host: String ,port: Int) {

  def run[R <: Console,T](processor: T => RIO[R, List[T]],
          encoder: T => RIO[R,Array[Byte]],
          decoder: Array[Byte] => RIO[R,T]): RIO[R, Unit] =
    server(host,port)
      .use(handleConnections(_, processor,decoder,encoder))



  private def server(host: String ,port: Int): Managed[Exception, AsynchronousServerSocketChannel] =
    for {
      server        <- AsynchronousServerSocketChannel()
      socketAddress <- SocketAddress.inetSocketAddress(host,port).toManaged_
      _             <- server.bind(socketAddress).toManaged_
    } yield server

  private def handleConnections[R<: Console,T]( server: AsynchronousServerSocketChannel,
                                 processor: T => RIO[R, List[T]],
                                 protocolDecoder: Array[Byte] => RIO[R,T],
                                 protocolEncoder: T => RIO[R,Array[Byte]]): RIO[R, Unit] =
    ZStream
      .repeat(server.accept)
      .flatMap(conn => ZStream.managed(conn.ensuring(console.putStrLn("Connection closed"))))
      .mapM { channel =>
        val r = for {
          _    <- console.putStrLn("Received connection")
          requestData <- channel.read(256)
          requestObject <- protocolDecoder(requestData.toArray)
          _    <- console.putStrLn("Data received: "+requestObject)
          respObjects    <- processor(requestObject)
          _    <- console.putStrLn("Response to be sent: "+respObjects)
          respData    <- respObjects.map(protocolEncoder).sequence
          writtenBytes <- respData.map(arr => channel.write(Chunk.fromArray(arr))).sequence
          _    <- console.putStrLn("Bytes sent: "+writtenBytes)
        } yield ()
        r.catchAllDefect(e=>URIO(e.printStackTrace()))
          .catchAll(e=>URIO(e.printStackTrace()))
      }.runDrain

}
