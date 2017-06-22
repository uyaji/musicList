package code.comet

import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.js.jquery._
import akka.actor._
import code.model._
import code.lib.BridgeController

class CometNotification extends CometActor {
  override def defaultPrefix = Full("twit")
  // totalは、メッセージの有無、話者の人数を反映させる。
  var total = ValueCell(100)
  // 現状roomは、固定だが、動的に変更する。
  // 呼び出すBridgeActorは、ログイン者毎のBridgeActorを新規で作成する。
  val room = "43"
  def render = bind("total" -> WiringUI.asText(total, JqWiringSupport.fade))

  override def lowPriority = {
    case msg: Message  => {
      total.set(total.get + 1)
    }
  }

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
}
