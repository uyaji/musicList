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
              case "0" =>(album.tracks.size + 1).toString
              case _ => getSeq()
            }
  var tracktitle: String = getSeq() match {
                     case "0" => ""
//                     case _   => Album.findAll(By(Album.id, albumid.toLong)).head.tracks.filter{ tr => tr.seq == getSeq().toLong}.head.tracktitle.get
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

  def render = {
    "name=albumid" #> SHtml.hidden( () => albumid) &
    "name=seq" #> SHtml.text( seq, seq = _) &
    "name=tracktitle" #> SHtml.text( tracktitle, tracktitle = _) &
    "name=upload" #> SHtml.fileUpload(s => upload = Full(s)) &
    "type=submit" #> SHtml.onSubmitUnit(process);
  }

  def process() {
    Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.seq == seq.toLong }.size match {
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
              track.attaches += attach
              track.save
              track
            }
            case _ => tracks.head
          }
          track.attaches += attach
          track.save
/*          val album: Album = Album.findAll(By(Album.id, albumid.toLong)).head
          album.tracks += track
          album.save
          val saveAlbum = Album.findAll(By(Album.id, albumid.toLong)).head
          saveAlbum.albumTracks.filter{atr => atr.track == track.id}.head.setSeq(seq.toLong)
          saveAlbum.save*/
          val albumTrack: AlbumTracks = AlbumTracks.create.album(albumid.toLong).track(track.id.get).seq(seq.toLong)
          albumTrack.save
          track
        }
        case false => {
          val tracks: List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
          val track: Track = tracks match {
            case Nil => Track.create.tracktitle(tracktitle)
            case _ => tracks.head
          }
          track.save
/*          val album: Album = Album.findAll(By(Album.id, albumid.toLong)).head
          album.tracks += track
          album.save*/
          val albumTrack: AlbumTracks = AlbumTracks.create.album(albumid.toLong).track(track.id.get).seq(seq.toLong)
          albumTrack.save
          track
        }
      }
      track.validate match{
        case Nil => {
          track.save
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
    val track = Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.seq == seq.toLong}.head.getTrack
    track.tracktitle(tracktitle)
    if(isAtachFileExist(upload)) {
      val attach = new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
      track.attaches += attach
    }
    track.save
    S.notice("Updateed " + track.tracktitle)
    S.redirectTo("track?albumid=" + albumid)
  }

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    album.tracks.flatMap(trk => {
      val trkSeq: String = trk.albumTracks.filter{atr => atr.album == getAlbumId().toLong }.head.seq.get.toString
      trk.attaches.size match {
        case 0 => {
          bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
             "seq" -> <span>{
                   link("track?albumid=" + getAlbumId() + "&seq=" + trkSeq , () => (), Text(trkSeq))
             }</span>,
             "tracktitle" -> <span>{
                   trk.tracktitle.toString
             }</span>,
             "filename" -> <span>{
                 Text(" ")
             }</span>,
             "delete" -> <span>{
                   link("track?albumid=" + getAlbumId(), () => delete(trk.id.get), Text("delete"))
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
                   link("track?albumid=" + getAlbumId() + "&seq=" +trkSeq, () => (), Text(trkSeq))
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
               i match {
                 case 1 =>
                   link("track?albumid=" + getAlbumId(), () => delete(trk.id.get), Text("delete"))
                 case _ =>
                   Text("")
               }
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

  def delete(id: Long): Unit ={
//     val attaches: List[Attach] = Attach.findAll(By(Attach.tracks, id))
//     attaches.map{ at => at.delete_! }
     val tracks: List[Track] = Track.findAll(By(Track.id, id))
     tracks.head.attaches.map{ at => at.delete_! }
     tracks.head.delete_!
     S.notice("Deleted " + tracks.head.tracktitle)
  }
}
