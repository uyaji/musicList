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
    case msg:List[Message] => {
      partialUpdate( PrependHtml(spanId,
        <xml:Group>
          { msg.map( m => {
            <ul class="status">
              <li class="message_l">{ m.status.is.get }</li>
              <li class="user_l">{ userName( m.from.obj ) }</li>
            </ul>
          })}
        </xml:Group>
      ))
    }
  }
  def userName( user:Box[User] ) = user.dmap( "Guest" ){ user => user.shortName }
}
