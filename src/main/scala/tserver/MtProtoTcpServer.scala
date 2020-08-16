package tserver


import java.nio.ByteBuffer

import scodec.bits.ByteVector
import tserver.common.ZioNioTcpServer
import tserver.common.ZioNioTcpServer.Result
import tserver.config.ServerConfig
import tserver.repos.{AuthState, NoRespSentState, ResPqSentState, StateRepo, StateService}
import tserver.utils.{Base64Utils, RsaUtil, Sha1Util}
import zio.clock.Clock
import zio.console.Console
import zio.duration.durationInt
import zio.{RIO, ZEnv, _}
import zio.interop.catz.{console => _, _}

import scala.util.Random

object MtProtoTcpServer extends App {

  type Env = Has[StateService] with Console with Clock

  def run(args: List[String]): URIO[ZEnv, ExitCode] = for {
    ref <- Ref.make(NoRespSentState:AuthState)
    layer = ZEnv.live ++ ZLayer.succeed(StateService(ref))
    server <- new ZioNioTcpServer(ServerConfig.port)
      .run(processor,serializer,deserializer)
      .provideLayer(layer)
      .exitCode
  } yield server

  def processor(proto: Protocol):RIO[Env, Result[List[Protocol]]] = proto match {
    case ReqPQ(aKey, _, _, _, n) =>
      val fingerprint = BigInt(Base64Utils.decodeFromString(ServerConfig.key1.publicKey).takeRight(8))
      val newPq = 46
      val response:Protocol = ResPQ(aKey,Random.nextLong,0,res_pq_constructor,n,n.reverse,newPq,vector_long_con,1,fingerprint)
      StateRepo.setAuthState(ResPqSentState(newPq)) *> // Simplification: no guaranty that resp wont fail
      RIO(Result(List(response),keepConnection = true))
    case ReqDHParams(_,_,_,_,_,_,p,q,keyFPrint,encData) => for {
        state <- StateRepo.getAuthState
        _ <- validatePqAndState(state,p,q)
        decArray <- RsaUtil.decrypt(encData.toArray,ServerConfig.key1.privateKey)
        (sha1,dirtyData) <- ZIO.effect(decArray.tail.splitAt(sha1Size)) // drop 256th byte and split
        _ <- console.putStrLn(s"SHA1: ${ByteVector(sha1)} Dirty data: ${ByteVector(dirtyData)}")
        cleanedData = dirtyData.take((PqInnerDataCodec.sizeBound.upperBound.get/8).toInt) // remove random bytes
        _ <- console.putStrLn(s"Cleaned data: ${ByteVector(cleanedData)}")
        _ <- validateSha1(cleanedData,sha1)
        innerObj <- ZIO.effect(PqInnerDataCodec.decode(ByteVector(cleanedData).toBitVector).require.value)
        _ <- console.putStrLn(s"server_DH_inner_data: $innerObj")
      } yield Result(List())
    case t =>
      ZIO.fail(new IllegalStateException(s"Processor error. Not supported type.$t"))
  }

  def serializer(arr: Protocol):RIO[Env, Array[Byte]] = arr match {
    case res: ResPQ => ZIO(ResPQCodec.encode(res).require.toByteArray)
    case e => ZIO.fail(new IllegalStateException(s"Following type is not supported: $e"))
  }

  def deserializer(arr: Array[Byte]):RIO[Env, Protocol]  = {
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

  def validateSha1(data: Array[Byte], sha1:Array[Byte]):RIO[Env,Unit] =
    if (Sha1Util.calculate(data) sameElements sha1)
      console.putStrLn(s"Valid sha1")
    else
      ZIO.fail(new IllegalStateException(s"Invalid sha1 for inner data encoded"))

  def validatePqAndState(state: AuthState, p:Long, q:Long):RIO[Env,Unit] = state match {
      case ResPqSentState(pq) =>
        if (BigInt(p*q)==pq) console.putStrLn(s"P/Q are valid")
        else ZIO.fail(new IllegalStateException(s"P/Q received are not valid"))
      case _ =>ZIO.fail(new IllegalStateException(s"Can't process ReqDHParams. Your must send ReqPQ first."))
  }

  val sha1Size = 20


}