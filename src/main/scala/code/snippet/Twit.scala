package code.snippet

import net.liftweb.actor._
import scala.xml.{NodeSeq,Text}
import akka.actor._
import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common.{Box, Full}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.jquery.JqJsCmds._
import code.lib._
import code.model._
import code.logic._
import net.liftweb.mapper._
import code.comet._

class Twit {
  try {
    // 既にsessinにCometTwitが登録されている場合、登録解除後、現在の相手で再登録
//    val comets = S.session.get.findComet("cometTwit")
    val comets = S.session.map(s => s.findComet("CometTwit"))
    // 登録されている会話相手(room)と、現在の会話相手(room)が異なる場合、
    // 登録解除、再登録。
    comets.map{cml => cml.map{cm =>
      val from = User.currentUser.head.id.get.toString
      val to = Util.paramGet("to")
      val room = if(from > to) from + to else to + from
      if(!cm.asInstanceOf[CometTwit].room.equals(room)) {
        cm.asInstanceOf[CometTwit].unSubScribe
        cm.asInstanceOf[CometTwit].changeRoom(Util.paramGet("to"))
      }
    }}
  } catch {
//    case e: Exception => println("Exception is happened")
    case e: Exception => e.printStackTrace()
  }
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
//        TwitServer ! message
        bridge ! message
        notifBridge ! message
        S.notice("twit the message")
        S.redirectTo(path)
      }
      case x => S.error( x )
    }

    val twitBind = {
      "#twitname" #> <span>{ name }</span> &
      "#twituploader" #> <span>{ User.findAll(By(User.id, to)).head.shortName }</span> &
      "#twitstatus" #> <span>{ message.status.toForm.head }</span> &
      "#twitsubmit" #> <span>{ submit("twit", () => addMessage ) }</span>
    }
    twitBind(xhtml)
/*    bind("twit", xhtml,
      "name" -> name,
      "uploader" -> User.findAll(By(User.id, to)).head.shortName,
      "status" -> message.status.toForm,
      "submit" -> submit("twit", () => addMessage )
    )*/
  }

  def show(xhtml:NodeSeq): NodeSeq = {
    val seq: Seq[Long] = List(from, to)
    <xml:Group>{
      Message.findAll(ByList(Message.from, seq), ByList(Message.to, seq), OrderBy(Message.id, Descending)).flatMap( msg => {
        if( msg.from.equals(User.currentUser.head.id.get)) {
          val twitBind = {
            "#twitmessage" #> <span><div class="balloon_l"><p>{msg.status.get}</p></div></span> &
            "#twituser" #> <span><li class="user_l">{userName(msg.fromUser)}</li></span>
          } 
          twitBind(xhtml)
/*            bind("twit", xhtml, 
//              "message" -> <li class="balloon_l">{msg.status.get}</li>,
              "message" -> <div class="balloon_l"><p>{msg.status.get}</p></div>,
              "user" -> <li class="user_l">{userName(msg.fromUser)}</li>
            )*/
        }
        else {
          val twitBind = {
            "#twitmessage" #> <span><div class="balloon_r"><p>{msg.status.get}</p></div></span> &
            "#twituser" #> <span><li class="user_r">{userName(msg.fromUser)}</li></span>
          }
          twitBind(xhtml)
/*            bind("twit", xhtml, 
//              "message" -> <li class="balloon_r">{msg.status.get}</li>,
              "message" -> <div class="balloon_r"><p>{msg.status.get}</p></div>,
              "user" -> <li class="user_r">{userName(msg.fromUser)}</li>
            )*/
        }
      })
    }</xml:Group>
  }

  def userName( user:User ) = user.shortName

  private lazy val bridge: ActorRef = {
    val room = if (from > to) from.toString+to.toString else to.toString + from.toString
    BridgeController.getBridgeActor(room)
  }

  private lazy val notifBridge: ActorRef = {
    BridgeNotifController.getBridgeActor(to.toString)
  }
}
