package code.snippet

import java.io._
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import code.model._
import code.logic._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

class TrackView extends PaginatorSnippet[AlbumTracks] {
  val albumid = Util.paramGet("albumid")
  val offset = Util.paramGet("offset")
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
  val authorityErrorMsg = "You are not the super user."

  override val itemsPerPage = 5
  override def pageUrl(offset: Long): String = appendParams( super.pageUrl(offset), List("albumid" -> albumid))
  override def count = AlbumTracks.findAll(By(AlbumTracks.album, album.id.get)).size
  override def page = AlbumTracks.findAll(By(AlbumTracks.album, album.id.get), OrderBy(AlbumTracks.seq, Ascending), StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

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
      val errMsgTrack = "Duplicate track!"
      val path = "/track?albumid=" + albumid + "&offset=" + offset
      val attach = isAttachFileExist(upload) match {
        case true => getExistAttach(getFileParamHolder(upload).fileName) match {
          case Nil => Some(new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file, Util.isSuperUser ))
          case attaches: List[Attach] => Some(attaches.head)
        }
        case false => None
      }
      Logic.select(duplicateSeqCheck, changeSeqCheck)(albumid.toLong, seq.toLong, albumtrcid.toLong, msg, path) match {
        case "add" => {
          val msg = Process.add(Logic.registTarget, duplicateKeyCheck, getExistTrack, tracktitle, album, Track.create.tracktitle(tracktitle), AlbumTracks.create.album(album.id.get).seq(seq.toLong), attach, 0, if(isAttachFileExist(upload) && !Util.isSuperUser) "we accept your upload request. please wait a moment." else "Added " + tracktitle, "Duplicate track!")
          S.error(msg)
        }
        case "update" => {
          val msg = Process.update(Logic.updateTarget, getTrack, getBinder, getExistTrack, isAttachFileExist, getExistAttach, tracktitle, seq.toLong, upload, album, albumtrcid.toLong, attach, if(isAttachFileExist(upload) && !Util.isSuperUser) "we accept your upload request. please wait a moment." else "Updated " + tracktitle, "Duplicate track!", "Duplicate attach!")
          S.error(msg)
        }
      }
      S.redirectTo(path)
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

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    def prevXml: NodeSeq = Text(?("<"))
    def nextXml: NodeSeq = Text(?(">"))
    def firstXml: NodeSeq = Text(?("<<"))
    def lastXml: NodeSeq = Text(?(">>"))
    def currentXml: NodeSeq = Text("Displaying records " + (first+1) + "-" + (first+itemsPerPage min count) + " of  " + count)
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    page.flatMap(albmtrack => {
      val trkSeq: String = albmtrack.seq.get.toString
      val albumtrcid: String = albmtrack.id.get.toString
      val track = Track.findAll(By(Track.id, albmtrack.track.get)).head
      track.attaches.size match {
        case 0 => {
          bind("track", html, AttrBindParam("id", albmtrack.track.get.toString, "id"),
             "seq" -> <span>{
                   link("track?albumid=" + Util.paramGet("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid + "&offset=" + offset, () => (), Text(trkSeq))
             }</span>,
             "tracktitle" -> <span>{
                   track.tracktitle.toString
             }</span>,
             "filename" -> <span>{
                 Text(" ")
             }</span>,
             "delete" -> <span>{
                   link("track?albumid=" + Util.paramGet("albumid") + "&offset=" + offset, () => delete(track.id.get,0), Text("delete"))
             }</span>
          );
        }
        case _ => {
          var i = 0
          track.attaches.flatMap(atc => {
            i = i + 1
            bind("track", html, AttrBindParam("id", track.id.toString, "id"),
             "seq" -> <span>{
               i match {
                 case 1 =>
                   link("track?albumid=" + Util.paramGet("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid + "&offset=" +  offset, () => (), Text(trkSeq))
                 case _ =>
                   Text("")
               }
             }</span>,
             "tracktitle" -> <span>{
               i match {
                 case 1 =>
                   track.tracktitle.toString
                 case _ =>
                   Text("")
               }
             }</span>,
             "filename" -> <span>{
               track.attaches.size match {
                 case 0 => Text(" ")
                 case _ => if(atc.valid.get || Util.isSuperUser) link("lob/" + atc.id.get.toString, () => (), Text(atc.filename.toString)) else Text(" ")
               }
             }</span>,
             "delete" -> <span>{
               link("track?albumid=" + Util.paramGet("albumid") + "&offset=" + offset, () => delete(track.id.get, atc.id.get), Text("delete"))
             }</span>,
             "stars" -> <span>{
               atc.users.size match {
                 case 0 => Text(" ")
                 case _ => Text(atc.users.size.toString)
               }
             }</span>,
             "valid" -> <span>{
               if(Util.isSuperUser) {
                 link("track?albumid=" + Util.paramGet("albumid") + "&offset=" + offset, () => switchShow(atc.id.get, !(atc.valid.get)), Text(if(atc.valid.get) "valid" else "invalid"))
               } else {
                 Text(" ")
               }
             }</span>,
             "addstar" -> <span>{
               link("track?albumid=" + Util.paramGet("albumid") + "&offset=" + offset, () => addStar(atc.id.get), Text("Add Start"))
             }</span>
            );
          })
        }
      }
    })
  }          
  
  private 
    def delete(trackid: Long, attachid: Long): Unit ={
      if(Util.isSuperUser) {
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
      } else {
        S.error(authorityErrorMsg)
      }
    }

    def switchShow(id: Long, valid: Boolean) {
      val attach = Attach.findAll(By(Attach.id, id)).head
      attach.valid(valid)
      attach.save
    }

    def addStar(id: Long) {
      val currentUser = User.currentUser.head
      val attach = Attach.findAll(By(Attach.id, id)).head
      if(!attach.users.contains(currentUser)) {
        attach.users += currentUser
        attach.save
        S.notice("You have added the star.")
      } else {
        S.error("You have already added.")
      }
    }

    def duplicateSeqCheck(albumid: Long, seq: Long): Boolean =
          Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.seq == seq }.size.equals(0)
    def changeSeqCheck(albumTrackId: Long, seq: Long): Boolean =
          AlbumTracks.findAll(By(AlbumTracks.id, albumTrackId)).head.seq.equals(seq)
    def getBinder(albumid: Long): Binder = Album.findAll(By(Album.id, albumid)).head
    def getTrack(albumid: Long, albumtrcid: Long): Track = Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.id == albumtrcid}.head.getTrack
//    def getExistTrack(tracktitle: String): List[Target] = Track.findAll(By(Track.tracktitle, tracktitle))
    def getExistTrack(tracktitle: String): List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
    def duplicateKeyCheck(track: Target, album: Binder): Boolean = Album.findAll(By(Album.id, album.getId)).head.tracks.toList.contains(track)
    def getExistAttach(fileName: String) = Attach.findAll(By(Attach.filename, fileName))
}
