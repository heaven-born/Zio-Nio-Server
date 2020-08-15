package tserver

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousSocketChannel
import java.security.MessageDigest

import scodec.bits.{Bases, BitVector, ByteVector}
import scodec.codecs.implicits.{implicitStringCodec => _, _}
import tserver.config.ServerConfig
import tserver.utils.{FactorizationUtil, RsaUtil}

object DemoClient {

  def main(args: Array[String]): Unit = {

   val port = 8080
   val host ="localhost"

   val runtime = zio.Runtime.default
   val client = AsynchronousSocketChannel.open
   val hostAddress = new InetSocketAddress(host, port)
   client.connect(hostAddress).get

   val nonce = ByteVector.fromValidHex("0x3E0549828CCA27E966B301A48FECE2FC",Bases.Alphabets.HexUppercase)
   val reqPqObj = ReqPQ(0,5901257869632771658L,20,req_pq_constructor,nonce)

   println("ReqPQ object: "+ reqPqObj)

   val reqPqEncoded = ReqPQCodec.encode(reqPqObj).require.toByteVector
   println("ReqPQ data vector: "+ reqPqEncoded)

   val writeResult1 = client.write(ByteBuffer.wrap(reqPqEncoded.toArray))
   println("ResPQ bytes written: " + writeResult1.get)

   val bufferToRead = ByteBuffer.allocate(1024)

   val resPqReadBytes = client.read(bufferToRead).get
   println("ResPQ read status: " + resPqReadBytes)


   val resPqVector = ByteVector(bufferToRead.array)
   println("ResPQ data: " + resPqVector)

   val resPqObject = ResPQCodec.decode(resPqVector.toBitVector).require.value

   println("Read message: " + resPqObject )

   println("Received prime number: " + resPqObject.pq )

   val p_q = FactorizationUtil.factorize(resPqObject.pq.toLong)
   println("Calculated prime factor: " + p_q )

   val nonceNew = ByteVector.fromValidHex("0x110549820101984966B301A48F111111",Bases.Alphabets.HexUppercase)
   val innerData = PqInnerData(p_q_inner_data_con,resPqObject.pq,p_q.head,p_q(1),nonce,resPqObject.server_nonce,nonceNew)

   println("Inner data object: " + innerData )

   val innerDataVector = PqInnerDataCodec.encode(innerData).require.toByteVector

   println("Inner data vector: " + innerDataVector )

   val digest = MessageDigest.getInstance("SHA-1");
   digest.reset();
   digest.update(innerDataVector.toBitVector.toByteArray);

   val sha1 = ByteVector(digest.digest())

   println("Inner data SHA1: " + sha1 )

   val sha1_innerDataVector = innerDataVector.splice(0,sha1)

   println("Inner data + SHA1 vector: " + sha1_innerDataVector )

   val paddedVector = sha1_innerDataVector.padRight(255)

   println("Inner data + SHA1 vector (padded): " + paddedVector )

   val server_rsa_public_key = ServerConfig.rsa_keys.valuesIterator.next().publicKey

   val encryptedData = runtime.unsafeRun(RsaUtil.encrypt(paddedVector.toArray,server_rsa_public_key))

   println("Encrypted inner data: " + ByteVector(encryptedData) )

   val reqDHParams = ReqDHParams(0,5901257869632771658L,20,req_dh_params_constructor,
    nonce,resPqObject.server_nonce,p_q.head,p_q(1),resPqObject.fingerprints,ByteVector(encryptedData))

   println("ReqDHParams object: " + reqDHParams )

   val reqDHParamsEncoded = ReqDHParamsCodec.encode(reqDHParams).require.toByteVector
   println("ReqDHParams data vector: "+ reqDHParamsEncoded)

   val reqDHParamsEncodedWriteResult = client.write(ByteBuffer.wrap(reqDHParamsEncoded.toArray))
   println("ReqDHParams bytes written: " + reqDHParamsEncodedWriteResult.get)

  }

}
