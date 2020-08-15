package tserver.utils

import scala.annotation.tailrec

object FactorizationUtil {

  //copy-paste form stackoverflow
  def factorize(x: Long): List[Long] = {
    @tailrec
    def foo(x: Long, a: Long = 2, list: List[Long] = Nil): List[Long] = a*a > x match {
      case false if x % a == 0 => foo(x / a, a    , a :: list)
      case false               => foo(x    , a + 1, list)
      case true                => x :: list
    }
    foo(x)
  }

}
