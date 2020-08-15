package tserver


import java.nio.ByteBuffer

import scodec.bits.{Bases, ByteVector}
import tserver.common.ZioNioTcpServer
import zio.console.Console
import zio.{RIO, ZEnv, _}
import zio.interop.catz.{console => _, _}

import scala.language.implicitConversions

object MtProtoTcpServer extends App {

  type Env = Has[StateService] with Console

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    for { ref <- Ref.make(Empty:AuthState)
         layer = Console.live ++ ZLayer.succeed(StateService(ref))
         server <- new ZioNioTcpServer("localhost",8080)
            .run(processor,encoder,decoder)
            .provideLayer(layer)
            .exitCode
        } yield server
  }

  def processor(proto: Protocol):RIO[Env, List[Protocol]] = {
    val nonce = ByteVector.fromValidHex("0x3E0549828CCA27E966B301A48FECE2FC",Bases.Alphabets.HexUppercase)
    val server_nonce = ByteVector.fromValidHex("0xA5CF4D33F4A11EA877BA4AA573907330",Bases.Alphabets.HexUppercase)
    val pq = BigInt("1724114033281923457")
    val fongerprint = BigInt("2408273976350192835")
    val resp = ResPQ(0,5901257890957871105L,64,res_pq_constructor,nonce,server_nonce,pq,vector_long_con,1,fongerprint)
    RIO(List[Protocol](resp))
  }

  def encoder(arr: Protocol):RIO[Env, Array[Byte]] = arr match {
    case res: ResPQ => ZIO(ResPQCodec.encode(res).require.toByteArray)
    case e => ZIO.fail(new IllegalStateException(s"Following type is not supported: $e"))
  }


  def decoder(arr: Array[Byte]):RIO[Env, Protocol]  = {
    val buffer = arr.slice(20,24)
    val constructor = ByteBuffer.wrap(buffer.padTo(8,0.toByte).reverse).getLong
    constructor match {
      case `req_pq_constructor` =>
        ZIO(ReqPQCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case `req_dh_params_constructor` =>
        ZIO(ReqDHParamsCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case c =>
        val hexConstructor = ByteVector.apply(buffer)
        ZIO.fail(new Exception(s"Following constructor is not supported HEX($hexConstructor)/DEC($c)"))
    }
  }

  // --- env for storing state --- //

  object StateService {
    def setAuthState(state: AuthState): URIO[Env,Unit] =
      ZIO.accessM[Env](_.get[StateService].authKeyState.set(state))

    def getAuthState(state: AuthState): URIO[Env,AuthState] =
      ZIO.accessM[Env](_.get[StateService].authKeyState.get)
  }

  case class StateService(authKeyState:Ref[AuthState])

  sealed trait AuthState
  case object Empty extends AuthState
  case object ReceivedReqPq extends AuthState
  case object ReceivedReqDHParams extends AuthState


}