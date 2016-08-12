package code.snippet

import java.io._
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import code.model.Track
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

class TrackView {
  val albumid = getAlbumId()
  var seq = getSeq() match {
              case "0" =>(Track.count(By(Track.albumid, albumid.toLong)) + 1).toString
              case _ => getSeq()
            }
  var tracktitle = getSeq() match {
                     case "0" => ""
                     case _   => Track.findAll(By(Track.albumid, albumid.toLong), By(Track.seq, getSeq().toLong)).head.tracktitle.get
                   }
  var upload: Box[FileParamHolder] = Empty
  val id = 0
  def list(html: NodeSeq): NodeSeq = {
    def renderRow(): NodeSeq = {
      def reDraw() = JsCmds.Replace("all_tracks",renderRow())
      var render = "#all_tracks * " #> ((n: NodeSeq) => doList(reDraw)(n))
      render(html)
    }
    renderRow()
  }

  def render = {
    "name=albumid" #> SHtml.hidden( () => albumid) &
    "name=seq" #> SHtml.text( seq, seq = _) &
    "name=tracktitle" #> SHtml.text( tracktitle, tracktitle = _) &
    "name=upload" #> SHtml.fileUpload(s => upload = Full(s)) &
    "type=submit" #> SHtml.onSubmitUnit(process);
  }

  def process() {
    Track.findAll(By(Track.albumid, albumid.toLong), By(Track.seq, seq.toLong)).size match {
      case 0 => addProcess()
      case _ => updateProcess()
    }
  }

  def getFileParamHolder(upload: Box[FileParamHolder]): FileParamHolder = upload match {
    case Full(FileParamHolder(name, mime, fileName, data)) => FileParamHolder(name, mime, fileName, data)
  }

  def isAtachFileExist(upload: Box[FileParamHolder]):Boolean = upload match {
    case Full(FileParamHolder(name, mime, fileName, data)) => true
    case _ => false
  }

  def addProcess() {
    try {
      val track: Track = isAtachFileExist(upload) match {
        case true => new Track(albumid.toLong, seq.toLong, tracktitle, getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
        case false => new Track(albumid.toLong, seq.toLong, tracktitle)
      }
      track.validate match{
        case Nil => {
          track.save()
          S.notice("Added " + track.tracktitle)
          S.redirectTo("/track?albumid=" + albumid)
        }
        case x => {
          S.error("Validation Error!")
          S.redirectTo("/track?albumid=" + albumid)
        }
      }
    } catch {
      case e: java.lang.NumberFormatException => {
        S.error("SEQ must be the number!")
        S.redirectTo("/track?albumid=" + albumid)
      }
    }
  }

  def updateProcess() {
    val track = Track.findAll(By(Track.albumid, albumid.toLong),  By(Track.seq, seq.toLong)).head
    track.tracktitle(tracktitle)
    if(isAtachFileExist(upload)) {
      track.filename(getFileParamHolder(upload).fileName)
      track.mimetype(getFileParamHolder(upload).mimeType)
      track.trackatach(getFileParamHolder(upload).file)
    }
    track.save()
    S.notice("Updateed " + track.tracktitle)
    S.redirectTo("track?albumid=" + albumid)
  }

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    val tracks:List[Track] = Track.findAll(By(Track.albumid, getAlbumId().toLong), OrderBy(Track.seq, Ascending))
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    tracks.flatMap(trk =>
      bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
                          "seq" -> <span>{link("track?albumid=" + getAlbumId() + "&seq=" +trk.seq.get, () => (), Text(trk.seq.toString))}</span>,
                          "tracktitle" -> <span>{trk.tracktitle.toString}</span>,
//                          "filename" -> <span>{link("track?albumid=" + getAlbumId(), () => download(trk.id.get), Text(trk.filename.toString))}</span>,
                          "filename" -> <span>{link("lob/" + trk.id.get.toString, () => (), Text(trk.filename.toString))}</span>,
                          "delete" -> <span>{link("track?albumid=" + getAlbumId(), () => delete(trk.id.get), Text("delete"))}</span>
      )
    )
  }          
  
  private def getAlbumId(): String = {
    val albumId = S.param("albumid") match {
                    case Full(id) => id
                    case _ => "1"
                  }
    albumId
  }

  private def getSeq(): String = {
    S.param("seq") match {
       case Full(id) => id
       case _ => "0"
    }
  }

  def delete(id: Long): Unit ={
     val tracks: List[Track] = Track.findAll(By(Track.id, id))
     tracks.head.delete_!
     S.notice("Deleted " + tracks.head.tracktitle)
  }
}
