package code.comet

import net.liftweb._
import http._
import SHtml._
import scala.xml.{ NodeSeq, Text }
import akka.actor._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.http.js.JE._
import code.model._
import code.logic._
import code.lib.BridgeController

class CometTwit extends CometActor {
  var to = Util.paramGet("to")
  override def defaultPrefix = Full("twit")
  private lazy val spanId = uniqueId + "_massages_span"
  private var msg: List[Message] = Nil
  var from = User.currentUser.head.id.get.toString
  var room = if(from > to) from + to else to + from

//  def render = bind("messages" -> <span id={spanId}><div></div></span>)
  def render = {
    val messageBinder = "#messages" #> <span id={spanId}><div></div></span>
    messageBinder
  }

  override def lowPriority = {
    case msg: Message  => {
      if( !msg.fromUser.id.get.equals(User.currentUser.head.id.get) 
        && msg.toUser.id.get.equals(User.currentUser.head.id.get)) {
        partialUpdate( PrependHtml(spanId,
          <xml:Group>
            <ul class="status">
              <div class="balloon_r"><p>{ msg.status.get }</p></div>
              <li class="user_r">{ userName( msg.fromUser ) }</li>
            </ul>
          </xml:Group>
        ))
      }
    }
  }
  def userName( user:User ) = user.shortName

  private lazy val bridge: ActorRef = {
    BridgeController.getBridgeActor(room)
  }

  override def localSetup {
    bridge ! Subscribe(this)
    super.localSetup()
  }

  override def localShutdown {
    bridge ! UnSubscribe(this)
    super.localShutdown()
  }

  def unSubScribe {
    BridgeController.getBridgeActor(room) ! UnSubscribe(this)
  }

  def changeRoom(to: String) {
    val from = User.currentUser.head.id.get.toString
    val room = if(from > to) from + to else to + from
    this.room = room
    BridgeController.getBridgeActor(room) ! Subscribe(this)
  }
}

case class Subscribe(comet: CometActor)
case class UnSubscribe(comet: CometActor)
