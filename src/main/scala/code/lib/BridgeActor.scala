package code.lib
import akka.actor._
import akka.pattern._
import net.liftweb.http.CometActor
import code.config._
import scala.concurrent.Await
import java.util.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import akka.util.Timeout

class BridgeActor extends Actor {
  private var target: List[Option[CometActor]] = Nil
  def receive = {
    case comet: CometActor => {
      target = Some(comet) +: target
    }
    case msg => {
      target.foreach(_.get ! msg)
    }
  }
}

object BridgeController {
  implicit val timeout = Timeout(5 seconds)
  def getBridgeActor: ActorRef = {
    try {
      val f = GlobalActorSystem.getActorSystem.actorSelection("akka://manager/user/BridgeActor")
      val actorRef = Await.result(f.resolveOne(), timeout.duration)
      return actorRef
    } catch {
      case e: akka.actor.ActorNotFound => {
        GlobalActorSystem.getActorSystem.actorOf(akka.actor.Props[BridgeActor], "BridgeActor")
      }
      case _: Throwable =>{null}
    }
  }
}

