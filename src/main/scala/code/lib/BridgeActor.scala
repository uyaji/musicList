package code.lib
import akka.actor._
import akka.pattern._
import net.liftweb.common.Loggable
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
      println("************* regist actor ********************=" + comet)
      target = Some(comet) +: target
      println("************* actor size********************=" + target.size)
    }
    case msg => {
      println("************* receive msg ******************** = " + target.size)
      target.foreach(_.get ! msg)
    }
  }
}

object BridgeController extends Loggable {
  implicit val timeout = Timeout(1 seconds)
  def getBridgeActor: ActorRef = {
    try {
//      val actorRef = GlobalActorSystem.getActorSystem.actorFor("/usr/BridgeActor")
      val f = GlobalActorSystem.getActorSystem.actorSelection("akka://manager/user/BridgeActor")
      if(f == null) println("**********NG") else println("*********OK" + f)
//      val future = akka.pattern.ask(actorRef, Ping())
//      val result = Await.result(future, timeout.duration).asInstanceOf[Ping]
//      result.asInstanceOf[ActorRef]
      val actorRef = Await.result(f.resolveOne(), timeout.duration)
println("get the actor = " + actorRef)
      return actorRef
    } catch {
      case te: TimeoutException => {
println("Timeout!!!!!!!")
        GlobalActorSystem.getActorSystem.actorOf(akka.actor.Props[BridgeActor], "BridgeActor")
      }
//      case e: Exception => {
      case e: akka.actor.ActorNotFound => {
println("Actor Not Found **************************NGNG")
        GlobalActorSystem.getActorSystem.actorOf(akka.actor.Props[BridgeActor], "BridgeActor")
      }
    }
  }
}

case class Ping()
