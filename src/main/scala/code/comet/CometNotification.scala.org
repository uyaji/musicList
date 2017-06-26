package code.comet

import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.js.jquery._
import akka.actor._
import code.model._
import code.logic._
import code.lib.BridgeNotifController

class CometNotification extends CometActor {
  override def defaultPrefix = Full("twit")
  // notificationは、メッセージの有無、話者の人数を反映させる。
  var notification = ValueCell("")
  // 現状roomは、固定だが、動的に変更する。
  // 呼び出すBridgeActorは、ログイン者毎のBridgeActorを新規で作成する。
  val to = User.currentUser.head.id.get.toString
  def render = bind("notification" -> WiringUI.asText( notification , JqWiringSupport.fade))

  override def lowPriority = {
    case msg: Message  => {
      val notifArray = notification.get.split("from ")
      Thread.sleep(10000)
      notifArray.size match {
        case 1 => {
          notification.set(getNotification)
        }
        case _ => {
          val messageExistCount = Message.findAllByPreparedStatement(con => con.connection.prepareStatement("SELECT * FROM message WHERE to_c = " + User.currentUser.head.id.get + " GROUP BY from_C")).size
          if(!notifArray(1).substring(0,1).toLong.equals(messageExistCount)) {
            notification.set(getNotification)
          }
        }
      }
    }
    case InitialNotification => {
      Thread.sleep(6000)
      notification.set(getNotification)
    }
  }
  
  def getNotification(): String = {
    val messageExist = Message.findAllByPreparedStatement(con => con.connection.prepareStatement("SELECT * FROM message WHERE to_c = " + User.currentUser.head.id.get + " GROUP BY from_C"))
    val returnMessage = "you got a message from " + messageExist.size  + " person" 
    messageExist.size match {
      case 0 => ""
      case 1 => returnMessage
      case _ => returnMessage + "s"
    }
  }

  private lazy val bridge: ActorRef = {
    BridgeNotifController.getBridgeActor(to)
  }

  override def localSetup {
    bridge ! this
    bridge ! InitialNotification
    super.localSetup()
  }

  override def localShutdown {
    super.localShutdown()
  }

}

case class InitialNotification()
