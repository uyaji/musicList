package code.lib

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
import code.model._

object Suggest {

  val languages = List(
    "C", "C++", "Clojure", "CoffeeScript",
    "Java", "JavaScript",
    "POP-11", "Prolog", "Python", "Processing",
    "Scala", "Scheme", "Smalltalk", "SuperCollider"
  )
  val titles = Album.findAll().map(a => a.albumtitle.get).toList

  def suggestion(key: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val json = Extraction.decompose(titles.filter(_.toLowerCase.startsWith(key.toLowerCase)))
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }

}
