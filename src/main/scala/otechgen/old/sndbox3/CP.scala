package otechgen.old.sndbox3

object ChatProtocol {
 
  val KIND_SEND_MSG = "send_msg"
  
  val KIND_RECEIVE_MSG = "receive_msg"
  
  val KIND_REJECTED = "rejected"
  
  case class CPPacket[T <: CPEvent](val kind: String, val msg: T)
  
  trait CPEvent
  
  case class CPSendMsg(val msg: String) extends CPEvent
  
  case class CPReceiveMsg(val login: String, val content: String, timestamp: String) extends CPEvent
  
  case class CPRejected(val reason: String) extends CPEvent
  
}