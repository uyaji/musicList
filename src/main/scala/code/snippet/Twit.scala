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
import code.logic._
import net.liftweb.mapper._
import code.comet._

class Twit {

  val from = User.currentUser.head.id.get
  val to = Util.paramGet("to").toLong
  val path = "twit?to=" + to
  def post( xhtml: NodeSeq): NodeSeq = {
    val user = User.currentUser
    val message = Message.create.from( from ).to( to )
    val name = User.currentUser.head.shortName

    def addMessage: Unit = message.validate match {
      case Nil => {
        message.save
        TwitServer ! message
        S.notice("twit the message")
        S.redirectTo(path)
      }
      case x => S.error( x )
    }

    bind("twit", xhtml,
      "name" -> name,
      "uploader" -> User.findAll(By(User.id, to)).head.shortName,
      "status" -> message.status.toForm,
      "submit" -> submit("twit", () => addMessage )
    )
  }


  def show(xhtml:NodeSeq): NodeSeq = {
    val seq: Seq[Long] = List(from, to)
    <xml:Group>{
      Message.findAll(ByList(Message.from, seq), ByList(Message.to, seq), OrderBy(Message.id, Descending)).flatMap( msg => {
        if( msg.from.equals(User.currentUser.head.id.get)) {
            bind("twit", xhtml, 
//              "message" -> <li class="balloon_l">{msg.status.get}</li>,
              "message" -> <div class="balloon_l"><p>{msg.status.get}</p></div>,
              "user" -> <li class="user_l">{userName(msg.fromUser)}</li>
            )
        }
        else {
            bind("twit", xhtml, 
              "message" -> <li class="balloon_r">{msg.status.get}</li>,
              "user" -> <li class="user_r">{userName(msg.fromUser)}</li>
            )
        }
      })
    }</xml:Group>
  }

  def userName( user:User ) = user.shortName
}
