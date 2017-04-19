package code.comet

import net.liftweb._
import http._
import SHtml._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.http.js.JE._
import code.model._

class CometTwit extends CometActor with CometListener {
  override def defaultPrefix = Full("twit")
  private lazy val spanId = uniqueId + "_massages_span"

  def render = bind("messages" -> <span id={spanId}><div></div></span>)

  protected def registerWith = TwitServer

  override def lowPriority = {
    case msg  => {
      partialUpdate( PrependHtml(spanId,
        <xml:Group>
          { msg.asInstanceOf[List[Message]].map( m => {
            if(userName( m.fromUser).equals(User.currentUser.head.shortName)) {
              <ul class="status">
                <li class="message_l">{ m.status.get }</li>
                <li class="user_l">{ userName( m.fromUser ) }</li>
              </ul>
            }
            else {
              <ul class="status">
                <li class="message_r">{ m.status.get }</li>
                <li class="user_r">{ userName( m.fromUser ) }</li>
              </ul>
            }
          })}
        </xml:Group>
      ))
    }
  }
  def userName( user:User ) = user.shortName
}
