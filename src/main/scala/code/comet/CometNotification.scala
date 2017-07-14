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
  var notificaterCount = ValueCell(0)
  var notificationMessage = ValueCell("")
  var notificationPerson = ValueCell("")
  // 現状roomは、固定だが、動的に変更する。
  // 呼び出すBridgeActorは、ログイン者毎のBridgeActorを新規で作成する。
  val to = User.currentUser.head.id.get.toString
  def render = bind("notification" -> WiringUI.asText( notificationMessage , JqWiringSupport.fade))

  override def lowPriority = {
    case msg: Message  => {
      Thread.sleep(10000)
      messageSet()
    }
    case InitialNotification => {
      Thread.sleep(6000)
      messageSet()
    }
  }
  
  def messageSet() {
    val count = Message.findAllByPreparedStatement(con => con.connection.prepareStatement("SELECT * FROM message WHERE to_c = " + User.currentUser.head.id.get + " GROUP BY from_C")).size
    notificaterCount.set(count)
    if(count > 0) {
      if(notificaterCount.get > 1) notificationPerson.set(S.loc("persons").get.toString)
      else notificationPerson.set(S.loc("person").get.toString)
      notificationMessage.set(S.loc("gotamessage").get.toString)
      notificationMessage.set(((notificationMessage lift notificaterCount){_ + _} lift notificationPerson){_ + _}.get)
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
