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
import scala.collection.mutable._
import code.comet._

class BridgeActor extends Actor {
  private var target: ListBuffer[Option[CometActor]] = new ListBuffer()
  def receive = {
    case Subscribe(comet) => {
      target += Some(comet)
    }
    case UnSubscribe(comet) => {
      target -= Some(comet)
    }
    case msg => {
      target.foreach(_.get ! msg)
    }
  }
}

object BridgeController {
  implicit val timeout = Timeout(5 seconds)
  def getBridgeActor(room: String): ActorRef = {
    try {
      val f = GlobalActorSystem.getActorSystem.actorSelection("akka://manager/user/" + room)
      val actorRef = Await.result(f.resolveOne(), timeout.duration)
      return actorRef
    } catch {
      case e: akka.actor.ActorNotFound => {
        GlobalActorSystem.getActorSystem.actorOf(akka.actor.Props[BridgeActor], room)
      }
      case _: Throwable =>{null}
    }
  }
}

