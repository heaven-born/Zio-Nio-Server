package tserver


import java.nio.ByteBuffer

import scodec.bits.{Bases, BitVector, ByteVector}
import tserver.common.ZioNioTcpServer
import tserver.config.ServerConfig
import tserver.utils.RsaUtil
import zio.console.Console
import zio.{RIO, ZEnv, _}
import zio.interop.catz.{console => _, _}

import scala.language.implicitConversions
import scala.util.Random

object MtProtoTcpServer extends App {

  type Env = Has[StateService] with Console

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    for { ref <- Ref.make(Empty:AuthState)
         layer = Console.live ++ ZLayer.succeed(StateService(ref))
         server <- new ZioNioTcpServer(8080)
            .run(processor,encoder,decoder)
            .provideLayer(layer)
            .exitCode
        } yield server
  }

  def processor(proto: Protocol):RIO[Env, List[Protocol]] = proto match {
    case ReqPQ(aKey, _, _, _, n) =>
      val fingerprint = ServerConfig.rsa_keys.keysIterator.next() // just pick one available in config
      val response = ResPQ(aKey,Random.nextLong,0,res_pq_constructor,n,n.reverse,46,vector_long_con,1,fingerprint)
      StateRepo.setAuthState(ReceivedReqPq) *>
        RIO(List(response))
    case ReqDHParams(_,_,_,_,_,_,_,_,keyFPrint,encData) =>
      for {
        pKey <- ZIO.effect(ServerConfig.rsa_keys(keyFPrint).privateKey)
        decArray <- RsaUtil.decrypt(encData.toArray,pKey)
        (sha1,dirtyData) <- ZIO.effect(decArray.splitAt(20))
        _ <- console.putStrLn(s"SHA1: ${ByteVector(sha1)} Dirty data: ${ByteVector(dirtyData)}")
        cleanedData = dirtyData.take(PqInnerDataCodec.sizeBound.upperBound.get.toInt) // remove random bytes
        _ <- console.putStrLn(s"Cleaned data: ${ByteVector(cleanedData)}")
        innerObj <- ZIO.effect(PqInnerDataCodec.decode(ByteVector(cleanedData).toBitVector).require.value)
        _ <- console.putStrLn(s"server_DH_inner_data: $innerObj")
        _ <- StateRepo.setAuthState(ReceivedReqDHParams)
      } yield List()
    case t =>
      ZIO.fail(new IllegalStateException(s"Processor error. Not supported type.$t"))
  }

  def encoder(arr: Protocol):RIO[Env, Array[Byte]] = arr match {
    case res: ResPQ => ZIO(ResPQCodec.encode(res).require.toByteArray)
    case e => ZIO.fail(new IllegalStateException(s"Following type is not supported: $e"))
  }


  // for simplicity I assumed that I can receive only one message per connection,
  // so no extra efforts required to split multiple messages.
  def decoder(arr: Array[Byte]):RIO[Env, Protocol]  = {
    val constructorBytes = arr.slice(20,24)
    val constructor = ByteBuffer.wrap(constructorBytes.padTo(8,0.toByte).reverse).getLong
    constructor match {
      case `req_pq_constructor` =>
        ZIO(ReqPQCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case `req_dh_params_constructor` =>
        ZIO(ReqDHParamsCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case c =>
        val hexConstructor = ByteVector(constructorBytes)
        ZIO.fail(new Exception(s"Following constructor is not supported HEX($hexConstructor)/DEC($c)"))
    }
  }

  // --- env for storing state --- //

  object StateRepo {
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