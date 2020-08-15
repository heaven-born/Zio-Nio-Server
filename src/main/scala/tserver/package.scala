
import scodec.{Codec, SizeBound, TransformSyntax}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{BitVectorCodec, bytes, constant, fixedSizeBytes, ignore, int, int32L, int64L, uint32L, uint8L, variableSizeBytes, variableSizeDelimited}

package object tserver {


  sealed trait Protocol

  val ReqPQCodec = (int64L :: int64L :: int32L :: uint32L :: nonceCodec).as[ReqPQ]
  val req_pq_constructor = 1615239032 //0x60469778

  case class ReqPQ(auth_key_id: Long,
                   message_id: Long,
                   message_length: Int,
                   req_pq_con: Long,
                   nonce: ByteVector) extends Protocol

  val ResPQCodec = (int64L :: int64L :: int32L :: uint32L :: nonceCodec ::
    nonceCodec :: pqCoded :: uint32L :: int32L :: fingerprintCodec).as[ResPQ]
  val res_pq_constructor = 85337187 //0x05162463

  case class ResPQ(auth_key_id: Long,
                   message_id: Long,
                   message_length: Int,
                   res_pq_con: Long ,
                   nonce: ByteVector,
                   server_nonce:ByteVector,
                   pq: BigInt,
                   vector_long_con: Long,
                   count:Int,
                   fingerprints: BigInt) extends Protocol

  val ReqDHParamsCodec = (int64L :: int64L :: int32L :: uint32L :: nonceCodec ::
    nonceCodec :: p_q_Coded :: p_q_Coded :: fingerprintCodec :: bytesAtMost(260)).as[ReqDHParams]
  val req_dh_params_constructor = 3608339646L //0xd712e4be
  val vector_long_con = 481674261 // 0x1cb5c415

  case class ReqDHParams(auth_key_id: Long,
                         message_id: Long,
                         message_length: Int,
                         req_dh_param_con: Long ,
                         nonce: ByteVector,
                         server_nonce:ByteVector,
                         p: Long,
                         q: Long,
                         public_key_fingerprint: BigInt,
                         encrypted_data: ByteVector) extends Protocol


  val PqInnerDataCodec = (uint32L :: pqCoded :: p_q_Coded :: p_q_Coded :: nonceCodec ::
    nonceCodec :: bytes(32) ).as[PqInnerData]
  val p_q_inner_data_con = 2211011308L //0x83c95aec

  case class PqInnerData (
      q_q_inner_con: Long ,
      pq: BigInt,
      p: Long,
      q: Long,
      nonce: ByteVector,
      server_nonce:ByteVector,
      nonce_new:ByteVector,
  ) extends Protocol

  // ----------------- codecs --------------------------------//

  def bytesAtMost(size: Int): Codec[ByteVector] = new Codec[ByteVector] {
    private val codec = bytes
    override def sizeBound = SizeBound.atMost(size * 8L)
    def encode(b: ByteVector) = codec.encode(b)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"bytesAtMost($size)"
  }

  private lazy val nonceCodec = bytes(16)

  private lazy val pqCoded = variableSizeBytes(uint8L,bytes(11),-3)
    .xmap[BigInt](bv=>BigInt(bv.toArray.prepended(0:Byte)),
                  bi=>ByteVector(bi.toByteArray.reverse.padTo[Byte](12,0).dropRight(1).reverse))

  private lazy val fingerprintCodec = bytes(8)
    .xmap[BigInt](bv=>BigInt(bv.toArray.prepended(0:Byte)),
                  bi=>ByteVector(bi.toByteArray.reverse.padTo[Byte](9,0).dropRight(1).reverse))

  private lazy val p_q_Coded = variableSizeBytes(uint8L,bytes(7),-3)
    .xmap[Long](bv=>bv.toLong(),l=>ByteVector.fromLong(l,4))

}

