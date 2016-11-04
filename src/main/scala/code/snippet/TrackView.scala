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
  val albumid = Param.get("albumid")
  val album = Album.findAll(By(Album.id, albumid.toLong)).head
  var seq = Generater.generateSeq(album.albumTracks.size, 
              () => album.albumTracks.reduceLeft((at1, at2) => if(at1.seq.get > at2.seq.get) at1 else at2).seq.get + 1,
              Param.get("seq"))
  val albumtrcid = Param.get("albumtrcid")
  var tracktitle: String = Param.get("seq") match {
                     case "0" => ""
                     case _   => album.albumTracks.filter{ atr => atr.seq == Param.get("seq").toLong}.head.getTrack.tracktitle.get
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
      Process.select(duplicateSeqCheck, changeSeqCheck)(albumid.toLong, seq.toLong, albumtrcid.toLong, msg, path) match {
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

  def isAtachFileExist(upload: Box[FileParamHolder]):Boolean = upload match {
    case Full(FileParamHolder(name, mime, fileName, data)) => true
    case _ => false
  }

  def addProcess() {
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
        track
      }
      case false => {
        val tracks: List[Track] = Track.findAll(By(Track.tracktitle, tracktitle))
        val track: Track = tracks match {
          case Nil => Track.create.tracktitle(tracktitle)
          case _ => tracks.head
        }
        track
      }
    }
    track.validate match{
      case Nil => {
        // 登録時にtrackの重複がないかチェック
        album.tracks.toList.contains(track) match {
          case true => {
            S.error("Duplicate track!")
            S.redirectTo("/track?albumid=" + albumid)
          }
          case false => {
            track.save
            val albumTrack: AlbumTracks = AlbumTracks.create.album(albumid.toLong).track(track.id.get).seq(seq.toLong)
            albumTrack.validate match {
              case Nil => {
                albumTrack.save
                S.notice("Added " + track.tracktitle)
                S.redirectTo("/track?albumid=" + albumid)
              }
              case errors => {
                S.error(errors)
                S.redirectTo("/track?albumid=" + albumid)
              }
            }
          }
        }
      }
      case errors => {
        S.error(errors)
        S.redirectTo("/track?albumid=" + albumid)
      }
    }
  }

  def updateProcess() {
    val msg = "Updateed " + tracktitle
    val errorMsg = "Duplicate track!"
    val path = "/track?albumid=" + albumid
    val attach = new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
    Process.updateTarget(getTarget, getParent, getRelation, getExistTarget, isAtachFileExist)(album.id.get, albumtrcid.toLong, tracktitle, path, msg, errorMsg, seq.toLong, upload, attach)
  }
/*  def updateProcess() {
    val track = Album.findAll(By(Album.id, albumid.toLong)).head.albumTracks.filter{ atr => atr.id == albumtrcid.toLong}.head.getTrack
    val albumTrack = AlbumTracks.findAll(By(AlbumTracks.id, albumtrcid.toLong)).head
    // 指定されたTrackが既存かどうかチェック。
    //   既存: AlbumTracksのアソシエーションの変更
    //   未存：Trackのtracktitleの更新
    // trackの重複チェック用フラグ:exist
    var exist = false
    // 入力されたtracktitleで,trackオブジェクトをインスタンス化
    val existTracks = Track.findAll(By(Track.tracktitle, tracktitle))
    // tracktitleの変更を確認
    track.tracktitle.equals(tracktitle) match {
      // 変更がなければ、重複問題なし 
      case true => exist = true
      // 変更の場合、重複の可能性あり 
      case _ => {
        // 入力trackオブジェクトの有無確認
        existTracks.size match {
          case 0 => exist = true
          case _ => exist = false
        }
      }
    }
    exist match {
      // 重複の問題がないので、tracktitleの変更、attachの追加。
      case true => {
        track.tracktitle(tracktitle)
      }
      // 重複の恐れあり。Album内のtrackをチェック
      case _ => {
        val album = Album.findAll(By(Album.id, albumid.toLong)).head
        album.tracks.toList.contains(existTracks.head) match {
          // 重複あり
          case true => {
            S.error("Duplicate track!")
            S.redirectTo("/track?albumid=" + albumid)
          }
          // 重複がないので、アソシエーションの変更。
          case false =>
            albumTrack.track(existTracks.head.id.get)
        }
      }
    }
    if(isAtachFileExist(upload)) {
      val attach = new Attach(getFileParamHolder(upload).fileName, getFileParamHolder(upload).mimeType, getFileParamHolder(upload).file)
      track.attaches += attach
    }
    track.save
    albumTrack.seq(seq.toLong)
    albumTrack.save
    S.notice("Updateed " + track.tracktitle)
    S.redirectTo("track?albumid=" + albumid)
  } */

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    bind("track", html, "albumid" -> <input type="text" name="albumid" class="column span-10"/>)
    album.tracks.flatMap(trk => {
      val trkSeq: String = trk.albumTracks.filter{atr => atr.album == Param.get("albumid").toLong }.head.seq.get.toString
      val albumtrcid: String = trk.albumTracks.filter{atr => atr.album == Param.get("albumid").toLong }.head.id.get.toString
      trk.attaches.size match {
        case 0 => {
          bind("track", html, AttrBindParam("id", trk.id.toString, "id"),
             "seq" -> <span>{
                   link("track?albumid=" + Param.get("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
             }</span>,
             "tracktitle" -> <span>{
                   trk.tracktitle.toString
             }</span>,
             "filename" -> <span>{
                 Text(" ")
             }</span>,
             "delete" -> <span>{
                   link("track?albumid=" + Param.get("albumid"), () => delete(trk.id.get,0), Text("delete"))
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
                   link("track?albumid=" + Param.get("albumid") + "&seq=" + trkSeq + "&albumtrcid=" + albumtrcid, () => (), Text(trkSeq))
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
               link("track?albumid=" + Param.get("albumid"), () => delete(trk.id.get, atc.id.get), Text("delete"))
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
            val album = Album.findAll(By(Album.id, Param.get("albumid").toLong)).head
            album.tracks -= track
            album.save
          }
        }
        case _ =>
      }
      S.notice("Deleted " + track.tracktitle)
    }

    def duplicateSeqCheck(albumid: Long, seq: Long): Boolean =
          Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.seq == seq }.size.equals(0)
    def changeSeqCheck(albumTrackId: Long, seq: Long): Boolean =
          AlbumTracks.findAll(By(AlbumTracks.id, albumTrackId)).head.seq.equals(seq)
    def getParent(albumid: Long): Parent = Album.findAll(By(Album.id, albumid)).head
    def getTarget(albumid: Long, albumtrcid: Long): Target = Album.findAll(By(Album.id, albumid)).head.albumTracks.filter{ atr => atr.id == albumtrcid}.head.getTrack
    def getRelation(albumtrcid: Long): Relation = AlbumTracks.findAll(By(AlbumTracks.id, albumtrcid)).head
    def getExistTarget(tracktitle: String): List[Target] = Track.findAll(By(Track.tracktitle, tracktitle))
}

object Process {
  def select(duplicateCheck: (Long, Long) => Boolean, changeKeyCheck: (Long, Long) => Boolean)(target: Long, key: Long, relationKey: Long, msg: String, path: String): String = {
    duplicateCheck(target, key) match {
      // seqの重複なし。
      case true => {
        relationKey match {
          // 新規登録
          case 0 => "add"
          // 既存データ表示からの変更登録。
          case _ => "update"
        }
      }
      // seqの重複あり。
      case _ => {
        // seqの変更が無ければ、更新。
        relationKey match {
          // 新規登録からの既存SEQへの変更。
          case 0 => {
            S.error(msg)
            S.redirectTo(path)
          }
          // 既存データ表示からの変更登録。
          case _ => {
            changeKeyCheck(relationKey, key) match {
              // seqの変更が無ければ、更新。
              case true => "update"
              case _ => {
                S.error(msg)
                S.redirectTo(path)
              }
            }
          }
        }
      }
    }
  }

  def updateTarget(getTarget: (Long, Long) => Target, getParent: Long => Parent, getRelation: Long => Relation, getExistTarget: String => List[Target], isAtachFileExist: Box[FileParamHolder] => Boolean)(parentId: Long, relationId: Long, name: String, path: String, msg: String, errorMsg: String, seq: Long, upload: Box[FileParamHolder], attach: Attach): Unit = {
    val target = getTarget(parentId, relationId)
    val relation = getRelation(relationId)
    // 指定されたtargetが既存かどうかチェック。
    //   既存: relationのアソシエーションの変更
    //   未存: targetのnameの更新
    var exist = false
    // 入力されたnameで、targetオブジェクトをインスタンス化
    val existTargets = getExistTarget(name)
    // nameの変更を確認
    target.getName.equals(name) match {
      // 変更が無ければ、重複問題なし
      case true => exist = true
      // 変更の場合、重複の可能性あり
      case _ => {
        // 入力targetオブジェクトの有無確認
        existTargets.size match {
          case 0 => exist = true
          case _ => exist = false
        }
      }
    }
    exist match {
      // 重複の問題がないので、nameの変更。
      case true => {
        target.setName(name)
      }
      // 重複の恐れあり。BandSeq内のplayerをチェック
      case _ => {
        val parent = getParent(parentId)
        parent.getTargets.contains(existTargets.head) match {
          // 重複あり
          case true => {
            S.error(errorMsg)
            S.redirectTo(path)
          }
          // 重複がないので、アソシエーションの変更。
          case false =>
            relation.setTarget(existTargets.head.getId)
        }
      }
    }
    if(isAtachFileExist(upload)) {
      target.setLob(attach)  
    }
    target.save
    relation.setSeq(seq)
    relation.save
    S.notice(msg)
    S.redirectTo(path)
  }
}

object Generater {
  def generateSeq(rowCount: Long, getMaxSeq: () => Long, paramSeq: String): String = {
    paramSeq match {
      case "0" => rowCount match {
        case 0 => "1"
        case _ => getMaxSeq().toString
      }
      case _ => paramSeq
    }
  }
}

object Param {
  def get(key: String): String = {
    S.param(key) match {
      case Full(value) => value
      case _ => "0"
    }
  }
}
