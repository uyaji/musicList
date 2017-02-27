package code.lib

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
import code.model._
import code.logic._

object Suggest {

  def suggestion(key: String, key2: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val titles = key2 match {
      case "searchAlbumtitle" =>Album.findAll().map(a => a.albumtitle.get).toList
      case "searchArtist" =>Band.findAll().map(b=> b.bandname.get).toList
      case _ => List()
    }
    val json = Extraction.decompose(titles.filter(_.toLowerCase.startsWith(key.toLowerCase)))
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }

}
