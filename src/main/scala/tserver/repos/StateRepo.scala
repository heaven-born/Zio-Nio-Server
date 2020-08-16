package tserver.repos

import zio.{Has, Ref, URIO, ZIO}

object StateRepo {

  type Env = Has[StateService]

  def setAuthState(state: AuthState): URIO[Env,Unit] =
    ZIO.accessM[Env](_.get[StateService].authKeyState.set(state))

  def getAuthState: URIO[Env,AuthState] =
    ZIO.accessM[Env](_.get[StateService].authKeyState.get)
}

case class StateService(authKeyState:Ref[AuthState])

sealed trait AuthState
case object EmptyState extends AuthState
case class ReqPqReceived(pq:BigInt) extends AuthState
