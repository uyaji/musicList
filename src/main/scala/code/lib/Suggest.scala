package code.lib

import scala.collection.mutable._
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
import code.model._

object Suggest {

  def suggestion(key: String, fieldName: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val titles = fieldName match {
      case "searchAlbumtitle" => Album.findAll(OrderBy(Album.albumtitle, Ascending)).map(a => a.albumtitle.get).toList
      case "searchArtist" | "artistname" => {
        val newTitles = new ListBuffer[BandJson]()
        for(band <- Band.findAll.filter(_.bandname.get.toLowerCase.startsWith(key.toLowerCase))) {
          val bandJson = BandJson(band.bandname.get, band.bandname.get, band.id.get)
          newTitles += bandJson
        }
          newTitles.sortWith(_.label < _.label)
      }
      case "searchTrack" => Track.findAll(OrderBy(Track.tracktitle, Ascending)).map(t=> t.tracktitle.get).toList
      case "searchPlayer" | "membername" => Player.findAll(OrderBy(Player.name, Ascending)).map(p=> p.name.get).toList
      case _ => List()
    }
    val json = fieldName match {
      case "searchArtist" | "artistname" => parse(Serialization.write(titles))
      case _ => Extraction.decompose(titles.asInstanceOf[List[String]].filter(_.toLowerCase.startsWith(key.toLowerCase)))
    }
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }

}

case class BandJson(label: String, value: String, id: Long)
