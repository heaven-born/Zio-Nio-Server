package tserver.utils

import java.util.Base64

object Base64Utils {

  private val decoder = Base64.getDecoder

  def decodeFromString(s: String) = {
    decoder.decode(s)
  }

}
