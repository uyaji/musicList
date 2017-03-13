package code.lib

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
import code.model._

object Select {

  def rtnOptionList(key: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val options = Band.findAll(By(Band.id, key.toLong)).head.bandSeqs.map(bs => bs.seq.get).toList
    val json = Extraction.decompose(options)
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }

  def rtnPlayerList(key1: String, key2: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val options = Album.findAll(By(Album.id, key1.toLong)).head.getBandSeq().players.withFilter{p => p.name.get.toLowerCase.indexOf(key2.toLowerCase) >= 0}.map(pl => pl.name.get).toList
    val json = Extraction.decompose(options)
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }

  def rtnTrackList(key1: String, key2: String): Box[LiftResponse] = {
    implicit val formats = DefaultFormats
    val options = Album.findAll(By(Album.id, key1.toLong)).head.tracks.withFilter{t => t.tracktitle.get.toLowerCase.indexOf(key2.toLowerCase) >= 0}.map(tr => tr.tracktitle.get).toList
    val json = Extraction.decompose(options)
    val headerContentType = "Content-Type"
    val headerJsonContentValue = "application/json"
    val jsonHeader = List((headerContentType, headerJsonContentValue))
    val jsonString = compactRender(json)
    Full(InMemoryResponse(jsonString.getBytes("UTF-8"), jsonHeader, S.responseCookies, 200))
  }
}
