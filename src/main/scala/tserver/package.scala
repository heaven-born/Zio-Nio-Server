
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{byte, bytes, int32L, int64L, variableSizeBytesLong}

package object tserver {

  sealed trait Protocol

  implicit val reqpqcodec = (int64L :: int64L :: int32L :: int32L :: bytes(16)).as[ReqPQ]

  case class ReqPQ(auth_key_id: Long,
                   message_id: Long,
                   message_length: Int,
                   req_pq_con: Int,
                   nonce: ByteVector) extends Protocol

  //val pqCodec = variableSizeBytesLong(long8L,int64L,3)
  implicit val ResPQCodec = (int64L :: int64L :: int32L :: int32L :: bytes(16) ).as[ResPQ]

  case class ResPQ(auth_key_id: Long,
                   message_id: Long,
                   message_length: Int,
                   res_pq_con: Int,
                   nonce: ByteVector,
                   //server_nonce:ByteVector,
                  // pq: Long
                  ) extends Protocol

  case class ReqDHParams() extends Protocol

}

