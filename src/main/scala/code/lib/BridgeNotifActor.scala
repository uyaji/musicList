package code.lib
import akka.actor._
import akka.pattern._
import net.liftweb.http.CometActor
import code.config._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import akka.util.Timeout
import scala.collection.immutable._
import code.comet._

class BridgeNotifActor extends Actor {
  private var target: Set[Option[CometActor]] = Set()
  def receive = {
    case comet: CometActor => {
      target = target + Some(comet)
    }
    case msg => {
      target.foreach(_.get ! msg)
    }
  }
}

object BridgeNotifController {
  implicit val timeout = Timeout(5 seconds)
  def getBridgeActor(to: String): ActorRef = {
    try {
      val f = GlobalActorSystem.getActorSystem.actorSelection("akka://manager/user/notif" + to + "/")
      Await.result(f.resolveOne(), timeout.duration)
    } catch {
      case e: akka.actor.ActorNotFound =>{
        GlobalActorSystem.getActorSystem.actorOf(akka.actor.Props[BridgeNotifActor], "notif" + to)
      }
      case _: Throwable => {
        null
      }
    }
  }
}
