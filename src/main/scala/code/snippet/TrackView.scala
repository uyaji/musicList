package code.snippet

import java.io._
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import code.model._
import code.logic.Logic
import code.logic.Util
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

class TrackView {
  val albumid = Util.paramGet("albumid")
  val album = Album.findAll(By(Album.id, albumid.toLong)).head
  var seq = Util.generateSeq(album.albumTracks.size, 
              () => album.albumTracks.reduceLeft((at1, at2) => if(at1.seq.get > at2.seq.get) at1 else at2).seq.get + 1,
              Util.paramGet("seq"))
  val albumtrcid = Util.paramGet("albumtrcid")
  var tracktitle: String = Util.paramGet("seq") match {
                     case "0" => ""
                     case _   => album.albumTracks.filter{ atr => atr.seq == Util.paramGet("seq").toLong}.head.getTrack.tracktitle.get
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
    try {
      val msg = "Can not register.Already exsist track. Please update"
      val path = "/track?albumid=" + albumid
      Logic.select(duplicateSeqCheck, changeSeqCheck)(albumid.toLong, seq.toLong, albumtrcid.toLong, msg, path) match {
        case "add" => addProcess()
        case "update" => updateProcess()
      }
    } catch {
      case e: java.lang.NumberFormatException => {
        S.error("SEQ must be the number!")
        S.redirectTo("/track?albumid=" + albumid)
      }
    }
  }

  def getFileParamHolder(upload: Box[FileParamHolder]): FileParamHolder = upload match {
    case Full(FileParamHolder(name, mime, fileName, data)) => FileParamHolder(name, mime, fileName, data)
  }

  def isAttachFileExist(upload: Box[FileParamHolder]):Boolean = upload match {
    case Full(FileParamHolder(name, mime, fileName, data)) => true
    case _ => false
  }

  def addProcess() {
    val generatedTrack = Track.create.tracktitle(tracktitle)
    val generatedAlbumTrack = AlbumTracks.create.album(album.id.get).seq(seq.toLong)
    val msg = "Added " + tracktitle
    val errMsg = "Duplicate track!"
    val path = "/track?albumid=" + albumid
    val track = getExistTrack(tracktitle) match {
      case Nil => generatedTrack
      case tracks: List[Track] => tracks.head
    }
    val attach = isAttachFileExist(upload) match {
      case true => getExistAttach(getFileParamHolder(upload).fileName) match {
        case Nil => new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
        case attaches: List[Attach] => attaches.head
      }
      case false => null
    }
    if(isAttachFileExist(upload)) track.attaches += attach
    Logic.registTarget(duplicateKeyCheck)(track, generatedAlbumTrack, album, msg, errMsg, path) match {
      case Nil =>{
        track.save
        generatedAlbumTrack.track(track.id.get)
        generatedAlbumTrack.save
        S.notice(msg)
        S.redirectTo(path)
      }
      case errors: List[FieldError] => {
        errors(0).field match {
          case null => S.error(errors(0).msg)
          case _ => S.error(errors)
        }
        S.redirectTo(path)
      }
    }
  }

  def updateProcess() {
    val msg = "Updateed " + tracktitle
    val errorMsgTrack = "Duplicate track!"
    val errorMsgAttach = "Duplicate attach!"
    val path = "/track?albumid=" + albumid
    val attach = isAttachFileExist(upload) match {
      case true => {
        val attach = Attach.findAll(By(Attach.filename, getFileParamHolder(upload).fileName))
        attach match {
          case Nil => new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
          case _ => attach.head
        }
      }
      case false => null
    }
    val result = Logic.updateTarget(getTrack, getBinder, getExistTrack)(album.id.get, albumtrcid.toLong, tracktitle)
    result.error match {
      case true => {
        S.error(errorMsgTrack)
        S.redirectTo(path)
      }
      case false => ()
    }
    val track = getTrack(album.id.get, albumtrcid.toLong)
    val albumTrack = track.getRelation(albumtrcid.toLong)
    val existTracks = getExistTrack(tracktitle)
    result.changeContent match {
      case "name" => {
        track.tracktitle(tracktitle)
      }
      case _ => {
        albumTrack.track(existTracks.head.getId)
      }
    }
    if(isAttachFileExist(upload)) {
      track.getLobs.contains(attach) match {
        // attachの重複エラー
        case true => {
          S.error(errorMsgAttach)
          S.redirectTo(path)
        }
        case false => {
          track.setLob(attach)
        }
      }
    }
    albumTrack.seq(seq.toLong)
    albumTrack.save
    track.save
    S.notice(msg)
    S.redirectTo(path)
  }

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    album.tracks.flatMap(trk => {
      val trkSeq: String = trk.albumTracks.filter{atr => atr.album == Util.paramGet("albumid").toLong }.head.seq.get.toString
      val albumtrcid: String = trk.albumTracks.filter{atr => atr.album == Util.paramGet("albumid").toLong }.head.id.get.toString
      trk.attaches.size match {
        case 0 => {
          bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
             "seq" -> <span>{
                   link("track?albumid=" + Util.paramGet("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
             }</span>,
             "tracktitle" -> <span>{
                   trk.tracktitle.toString
             }</span>,
             "filename" -> <span>{
                 Text(" ")
             }</span>,
             "delete" -> <span>{
                   link("track?albumid=" + Util.paramGet("albumid"), () => delete(trk.id.get,0), Text("delete"))
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
                   link("track?albumid=" + Util.paramGet("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
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
               link("track?albumid=" + Util.paramGet("albumid"), () => delete(trk.id.get, atc.id.get), Text("delete"))
             }</span>
            );
          })
        }
      }
    })
  }          
  
  private 
    def delete(trackid: Long, attachid: Long): Unit ={
      // attchを削除した結果、attach recordが存在しなければ、trackを削除
      val attaches: List[Attach] = Attach.findAll(By(Attach.id, attachid))
      attaches.size match {
        case 0 =>
        case _ => attaches.head.delete_!
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
          }
          val album = Album.findAll(By(Album.id, Util.paramGet("albumid").toLong)).head
          album.tracks -= track
          album.save
        }
        case _ => ()
      }
      S.notice("Deleted " + track.tracktitle)
    }

    def duplicateSeqCheck(albumid: Long, seq: Long): Boolean =
          Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.seq == seq }.size.equals(0)
    def changeSeqCheck(albumTrackId: Long, seq: Long): Boolean =
          AlbumTracks.findAll(By(AlbumTracks.id, albumTrackId)).head.seq.equals(seq)
    def getBinder(albumid: Long): Binder = Album.findAll(By(Album.id, albumid)).head
    def getTrack(albumid: Long, albumtrcid: Long): Track = Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.id == albumtrcid}.head.getTrack
//    def getExistTrack(tracktitle: String): List[Target] = Track.findAll(By(Track.tracktitle, tracktitle))
    def getExistTrack(tracktitle: String): List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
    def duplicateKeyCheck(track: Target): Boolean = Album.findAll(By(Album.id, album.id.get)).head.tracks.toList.contains(track)
    def getExistAttach(fileName: String) = Attach.findAll(By(Attach.filename, fileName))
}
