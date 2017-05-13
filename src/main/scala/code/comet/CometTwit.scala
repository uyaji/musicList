package code.comet

import net.liftweb._
import http._
import SHtml._
import scala.xml.{ NodeSeq, Text }
import akka.actor._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.http.js.JE._
import code.model._
import code.lib.BridgeController

class CometTwit extends CometActor {
  bridge ! this
  override def defaultPrefix = Full("twit")
  private lazy val spanId = uniqueId + "_massages_span"
  private var msg: List[Message] = Nil

  def render = bind("messages" -> <span id={spanId}><div></div></span>)

  override def lowPriority = {
    case msg: Message  => {
      if( !msg.fromUser.id.get.equals(User.currentUser.head.id.get) ) {
        partialUpdate( PrependHtml(spanId,
          <xml:Group>
            <ul class="status">
              <li class="balloon_r">{ msg.status.get }</li>
              <li class="user_r">{ userName( msg.fromUser ) }</li>
            </ul>
          </xml:Group>
        ))
      }
    }
  }
  def userName( user:User ) = user.shortName

  private lazy val bridge: ActorRef = BridgeController.getBridgeActor
}
