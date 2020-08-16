package tserver.utils

import java.security.MessageDigest

object Sha1Util {

  def calculate(inputArray: Array[Byte]):Array[Byte] =  {
    val digest = MessageDigest.getInstance("SHA-1");
    digest.reset();
    digest.update(inputArray);
    digest.digest()
  }

}
