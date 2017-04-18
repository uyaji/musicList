package code.comet

import net.liftweb.actor._
import net.liftweb._
import net.liftweb.common._
import http._
import code.model._

object TwitServer extends LiftActor with ListenerManager {
  private var msgs: List[Message] = Nil
  protected def createUpdate = msgs

  override def lowPriority = {
    case m: Message => {
      msgs ::= m
      updateListeners()
    }
  }
}

case class Messages(msgs: List[Message])
