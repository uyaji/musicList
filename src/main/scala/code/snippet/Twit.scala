package code.snippet

import net.liftweb.actor._
import scala.xml.{NodeSeq,Text}
import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common.{Box, Full}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.jquery.JqJsCmds._
import code.model._
import net.liftweb.mapper._
import code.comet._

class Twit {

  def post( xhtml: NodeSeq): NodeSeq = {
    val user = User.currentUser
    val message = Message.create.from( user )
    val name = User.currentUser.head.shortName

    def addMessage: Unit = message.validate match {
      case Nil => {
        message.save
        TwitServer ! message
        S.notice("メッセージを投稿しました。")
      }
      case x => S.error( x )
    }

    bind("twit", xhtml,
      "name" -> name,
      "status" -> message.status.toForm,
      "submit" -> submit("投稿する", () => addMessage )
    )
  }


  def show(xhtml:NodeSeq): NodeSeq = {
    <xml:Group>{
      Message.findAll(OrderBy(Message.id, Descending)).flatMap( msg => {
        if(userName( msg.fromUser ).equals(User.currentUser.head.shortName)) {
            bind("twit", xhtml, 
              "message" -> <li class="message_l">{msg.status.get}</li>,
              "user" -> <li class="user_l">{userName(msg.fromUser)}</li>
            )
        }
        else {
            bind("twit", xhtml, 
              "message" -> <li class="message_r">{msg.status.get}</li>,
              "user" -> <li class="user_r">{userName(msg.fromUser)}</li>
            )
        }
      })
    }</xml:Group>
  }

  def userName( user:User ) = user.shortName
}
