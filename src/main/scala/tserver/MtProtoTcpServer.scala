package tserver


import java.nio.ByteBuffer

import scodec.bits.ByteVector
import tserver.common.ZioNioTcpServer
import tserver.common.ZioNioTcpServer.Result
import tserver.config.ServerConfig
import tserver.utils.RsaUtil
import zio.clock.Clock
import zio.console.Console
import zio.duration.durationInt
import zio.{RIO, ZEnv, _}
import zio.interop.catz.{console => _, _}

import scala.util.Random

object MtProtoTcpServer extends App {

  type Env = Has[StateService] with Console with Clock

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    for { ref <- Ref.make(NoRequestReceived:AuthState)
         layer = ZEnv.live ++ ZLayer.succeed(StateService(ref))
         server <- new ZioNioTcpServer(8080)
            .run(processor,encoder,decoder)
            .provideLayer(layer)
            .exitCode
        } yield server
  }

  def processor(proto: Protocol):RIO[Env, Result[List[Protocol]]] = proto match {
    case ReqPQ(aKey, _, _, _, n) =>
      //TODO: fix fingerpint
      val fingerprint = ServerConfig.rsa_keys.keysIterator.next() // just pick one available in config
      val response = ResPQ(aKey,Random.nextLong,0,res_pq_constructor,n,n.reverse,46,vector_long_con,1,fingerprint)
      StateRepo.setAuthState(ReceivedReqPq) *>
        RIO(Result(List(response),keepConnection = true))
    case ReqDHParams(_,_,_,_,_,_,p,q,keyFPrint,encData) =>
      val Sha1Size = 20
      for {
        isValidState <- StateRepo.getAuthState.map(_ == ReceivedReqPq)
        _ <- if (isValidState) ZIO.succeed(())
             else ZIO.fail(new IllegalStateException(s"Can't process ReqDHParams. Your must send ReqPQ first."))
        //TODO: add pq validation
        pKey <- ZIO.effect(ServerConfig.rsa_keys(keyFPrint).privateKey)
        decArray <- RsaUtil.decrypt(encData.toArray,pKey)
        (sha1,dirtyData) <- ZIO.effect(decArray.tail.splitAt(Sha1Size)) // drop firs byte and split
        //TODO: add sha1 validation
        _ <- console.putStrLn(s"SHA1: ${ByteVector(sha1)} Dirty data: ${ByteVector(dirtyData)}")
        cleanedData = dirtyData.take((PqInnerDataCodec.sizeBound.upperBound.get/8).toInt) // remove random bytes
        _ <- console.putStrLn(s"Cleaned data: ${ByteVector(cleanedData)}")
        innerObj <- ZIO.effect(PqInnerDataCodec.decode(ByteVector(cleanedData).toBitVector).require.value)
        _ <- console.putStrLn(s"server_DH_inner_data: $innerObj")
      } yield Result(List())
    case t =>
      ZIO.fail(new IllegalStateException(s"Processor error. Not supported type.$t"))
  }

  def encoder(arr: Protocol):RIO[Env, Array[Byte]] = arr match {
    case res: ResPQ => ZIO(ResPQCodec.encode(res).require.toByteArray)
    case e => ZIO.fail(new IllegalStateException(s"Following type is not supported: $e"))
  }


  //TODO: add scodec.codecs.ascii32 serialization
  def decoder(arr: Array[Byte]):RIO[Env, Protocol]  = {
    val constructorBytes = arr.slice(20,24)
    val constructorLong = ByteBuffer.wrap(constructorBytes.padTo(8,0.toByte).reverse).getLong
    constructorLong match {
      case `req_pq_constructor` =>
        ZIO(ReqPQCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case `req_dh_params_constructor` =>
        ZIO(ReqDHParamsCodec.decode(ByteVector.apply(arr).toBitVector).require.value)
      case c =>
        val hexConstructor = ByteVector(constructorBytes)
        ZIO.fail(new IllegalStateException(s"Following constructor is not supported HEX($hexConstructor)/DEC($c)"))
    }
  }

  // --- env for storing state --- //

  object StateRepo {
    def setAuthState(state: AuthState): URIO[Env,Unit] =
      ZIO.accessM[Env](_.get[StateService].authKeyState.set(state))

    def getAuthState: URIO[Env,AuthState] =
      ZIO.accessM[Env](_.get[StateService].authKeyState.get)
  }

  case class StateService(authKeyState:Ref[AuthState])

  sealed trait AuthState
  case object NoRequestReceived extends AuthState
  case object ReceivedReqPq extends AuthState


}