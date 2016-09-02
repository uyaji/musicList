package code.snippet

import java.io._
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import code.model._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

class TrackView {
  val albumid = getAlbumId()
  val album = Album.findAll(By(Album.id, albumid.toLong)).head
  var seq = getSeq() match {
              case "0" =>(album.albumTracks.reduceLeft((at1, at2) => if(at1.seq.get > at2.seq.get) at1 else at2).seq.get + 1).toString
              case _ => getSeq()
            }
  val albumtrcid = getAlbumTrcId()
  var tracktitle: String = getSeq() match {
                     case "0" => ""
                     case _   => Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.seq == getSeq().toLong}.head.getTrack.tracktitle.get
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

  def albumtitle(html: NodeSeq): NodeSeq = {
    bind("album", html, "title" -> album.albumtitle)
  }

  def render = {
    "name=albumid" #> SHtml.hidden( () => albumid) &
    "name=albumtrcid" #> SHtml.hidden( () => albumtrcid) &
    "name=seq" #> SHtml.text( seq, seq = _) &
    "name=tracktitle" #> SHtml.text( tracktitle, tracktitle = _) &
    "name=upload" #> SHtml.fileUpload(s => upload = Full(s)) &
    "type=submit" #> SHtml.onSubmitUnit(process);
  }

  def process() {
    albumtrcid match {
      case "0" => {
        Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.seq == seq.toLong }.size match {
          case 0 => addProcess()
          case _ => updateProcess()
        }
      }
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
        case true => {
          val attaches = Attach.findAll(By(Attach.filename, getFileParamHolder(upload).fileName))
          val attach: Attach = attaches match {
            case Nil => new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
            case _ => attaches.head
          }
          val tracks: List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
          val track: Track = tracks match {
            case Nil => {
              val track = Track.create.tracktitle(tracktitle)
              track
            }
            case _ => tracks.head
          }
          track.attaches += attach
/*          val album: Album = Album.findAll(By(Album.id, albumid.toLong)).head
          album.tracks += track
          album.save
          val saveAlbum = Album.findAll(By(Album.id, albumid.toLong)).head
          saveAlbum.albumTracks.filter{atr => atr.track == track.id}.head.setSeq(seq.toLong)
          album.albumTracks.filter{atr => atr.track == track.id}.head.setSeq(seq.toLong)
          saveAlbum.save*/
          track
        }
        case false => {
          val tracks: List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
          val track: Track = tracks match {
            case Nil => Track.create.tracktitle(tracktitle)
            case _ => tracks.head
          }
/*          val album: Album = Album.findAll(By(Album.id, albumid.toLong)).head
          album.tracks += track
          album.save*/
          track
        }
      }
      track.validate match{
        case Nil => {
          track.save
          val albumTrack: AlbumTracks = AlbumTracks.create.album(albumid.toLong).track(track.id.get).seq(seq.toLong)
          albumTrack.save
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
    val track = Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.id == albumtrcid.toLong}.head.getTrack
    track.tracktitle(tracktitle)
    if(isAtachFileExist(upload)) {
      val attach = new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
      track.attaches += attach
    }
    track.save
    val albumTrack = AlbumTracks.findAll(By(AlbumTracks.album, albumid.toLong), By(AlbumTracks.track, track.id.get)).head
    albumTrack.seq(seq.toLong)
    albumTrack.save
    S.notice("Updateed " + track.tracktitle)
    S.redirectTo("track?albumid=" + albumid)
  }

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    album.tracks.flatMap(trk => {
      val trkSeq: String = trk.albumTracks.filter{atr => atr.album == getAlbumId().toLong }.head.seq.get.toString
      val albumtrcid: String = trk.albumTracks.filter{atr => atr.album == getAlbumId().toLong }.head.id.get.toString
      trk.attaches.size match {
        case 0 => {
          bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
             "seq" -> <span>{
                   link("track?albumid=" + getAlbumId() + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
             }</span>,
             "tracktitle" -> <span>{
                   trk.tracktitle.toString
             }</span>,
             "filename" -> <span>{
                 Text(" ")
             }</span>,
             "delete" -> <span>{
                   link("track?albumid=" + getAlbumId(), () => delete(trk.id.get,0), Text("delete"))
             }</span>
          );
        }
        case _ => {
          var i = 0
          trk.attaches.flatMap(atc => {
            i = i + 1
            bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
             "seq" -> <span>{
               i match {
                 case 1 =>
                   link("track?albumid=" + getAlbumId() + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
                 case _ =>
                   Text("")
               }
             }</span>,
             "tracktitle" -> <span>{
               i match {
                 case 1 =>
                   trk.tracktitle.toString
                 case _ =>
                   Text("")
               }
             }</span>,
             "filename" -> <span>{
               trk.attaches.size match {
                 case 0 => Text(" ")
                 case _ => link("lob/" + atc.id.get.toString, () => (), Text(atc.filename.toString))
               }
             }</span>,
             "delete" -> <span>{
               link("track?albumid=" + getAlbumId(), () => delete(trk.id.get, atc.id.get), Text("delete"))
             }</span>
            );
          })
        }
      }
    })
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

  private def getAlbumTrcId(): String = {
    S.param("albumtrcid") match {
       case Full(id) => id
       case _ => "0"
    }
  }

  def delete(trackid: Long, attachid: Long): Unit ={
     // attchを削除した結果、attach recordが存在しなければ、trackを削除
     val attaches: List[Attach] = Attach.findAll(By(Attach.id, attachid))
     attaches.size match {
       case 0 =>
       case _ => val result = attaches.head.delete_!
     }
     val comAttaches: List[Attach] = Attach.findAll(By(Attach.track, trackid))
     val track: Track = Track.findAll(By(Track.id, trackid)).head
     comAttaches.size match {
       case 0 => {
         track.albums.size match {
           case 1 => {
             track.delete_!
           }
           case _ => ()
           val album = Album.findAll(By(Album.id, getAlbumId().toLong)).head
           album.tracks -= track
           album.save
         }
       }
       case _ =>
     }
     S.notice("Deleted " + track.tracktitle)
  }
}
