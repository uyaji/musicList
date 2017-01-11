package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import Helpers._

import code.model._
import code.logic._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import java.util.Date

class AlbumView {
  def list(html: NodeSeq): NodeSeq = {
    def renderRow(): NodeSeq = {
      def reDraw() = JsCmds.Replace("all_albums",renderRow())
      var render = "#all_albums * " #> ((n: NodeSeq) => doList(reDraw)(n))
      render(html)
    }
    renderRow()
  }

  def add(from : NodeSeq): NodeSeq = {
    val albumid = Util.paramGet("albumid").toLong
    var artistname = ""
    var albumtitle = ""
    var artistseq = ""
    albumid match {
      case 0 => ()
      case aid => {
        val album = Album.findAll(By(Album.id, aid.toLong)).head
        albumtitle = album.albumtitle.get
        val bandseq = BandSeq.findAll(By(BandSeq.id, album.bandseq.get)).head
        val band = Band.findAll(By(Band.id, bandseq.band.get)).head
        artistname = band.bandname.get
        artistseq = bandseq.seq.get.toString
      }
    }
    def albumProcess() {
      val errorMsg = "Can not register. Already exist Album. Please update"
      val path = "/"
      val regSeq = Util.isAllDigits(artistseq) match {
        case true => artistseq.toInt
        case _ => 1
      }
      val process = Logic.select(duplicateAlbumCheck, _==_ )(albumid, albumid, albumid, errorMsg, path)
      process match {
        case "add" => {
          Process.add(Logic.registTarget, duplicateKeyCheck, title => Nil, albumtitle, generateBand(artistname, regSeq), new Album(albumtitle).bandseq(generateBand(artistname, regSeq).bandSeqs.filter{bsq => bsq.seq==regSeq}.head.id.get), null, None, regSeq, "added " + albumtitle, errorMsg, path)
        }
        case "update" => albumToDataBase(id => Album.findAll(By(Album.id, id.toLong)).head, albumid.toString, artistname, artistseq, albumtitle, process, errorMsg)
      }
      S.mapSnippet("AlbumView.add", doBind)
    }

    def albumToDataBase(f: String => Album, key: String, artistName: String, artistSeq: String, albumTitle: String, process: String, errorMsg: String) = {
      val regSeq = Util.isAllDigits(artistSeq) match {
        case true => artistSeq.toInt
        case _ => 1
      }
      val band: Band = generateBand2(artistName, regSeq)
      val bandSeq = generateBand2Seq(band, regSeq)

      band.validate match {
        case Nil => ()
        case errors => {
          S.error(errors)
          S.redirectTo("/")
        }
      }
      bandSeq.validate match {
        case Nil => ()
        case errors => {
          S.error(errors)
          S.redirectTo("/")
        }
      }

      var album: Album = f(key)
      album.validate match{
        case Nil => ()
        case errors => S.error(errors); S.redirectTo("/")
      }
      if(!band.bandSeqs.forall(bseq => bseq.albums.forall(alb => alb.albumtitle != albumTitle))) {
        if(process.equals("update")) {
          if(!album.albumtitle.equals(albumTitle) || !album.getBandSeq().getBand().bandname.equals(artistName)) {
            S.error(errorMsg); S.redirectTo("/")
          }
        } else {
          S.error(errorMsg); S.redirectTo("/")
        }
      }
      band.bandSeqs += bandSeq
      band.save
      if(process.equals("update")) album.getBandSeq().albums -= album
      bandSeq.albums += album
      if(process.equals("update")) album.albumtitle(albumTitle)
      bandSeq.save()
      S.notice(process + "ed " + albumTitle)
      S.redirectTo("/")
    }

    def doBind(from: NodeSeq): NodeSeq = {
      var sel =
        "name=albumtitle" #> SHtml.text(albumtitle, albumtitle = _) &
        "name=artistname" #> SHtml.text(artistname, artistname = _) &
        "name=artistseq"  #> SHtml.text(artistseq, artistseq = _) &
        "type=submit" #> SHtml.onSubmitUnit(albumProcess);
      return sel(from)
    }

    doBind(from)
  }

  
  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    var albums:List[Album] = Album.findAll(OrderBy(Album.albumtitle, Ascending))
    var seq: Int = 0
    albums.flatMap(alb => {
      seq = seq + 1
      bind("album", html, AttrBindParam("id", alb.id.toString, "id"),
                          "seq" -> <span>{link("/?albumid=" + alb.id.toString, () => (), Text(seq.toString))}</span>,
                          "albumtitle" -> <span>{link("track?albumid=" + alb.id.toString, () => (), Text(alb.albumtitle.get))}</span>,
                          "artistname" -> <span>{link("band?bandid="+ alb.getBandSeq().getBand().id.toString, () => (), Text(alb.getBandSeq().getBand().bandname.get))}</span>,
                          "artistseq" -> <span>{link("member?bandid="+ alb.getBandSeq().getBand().id.toString + "&seq=" + alb.getBandSeq().seq.toString, () => (), Text(alb.getBandSeq().seq.toString))}</span>
      )
    })
  }          

  private def generateBand(artistName: String, regSeq: Int): Band = {
    val bands = Band.findAll(By(Band.bandname,artistName)) 
    var band = bands match {
      case Nil => {
        new Band(artistName)
      }
      case _ => bands.head
    }
    band = generateBandSeq(band, regSeq)
    band
  }

  private def generateBand2(artistName: String, regSeq: Int): Band = {
    val bands = Band.findAll(By(Band.bandname,artistName)) 
    bands match {
      case Nil => {
        new Band(artistName)
      }
      case _ => bands.head
    }
  }

  private def generateBandSeq(band: Band, seq: Int): Band = {
    val bandSeqs = BandSeq.findAll(By(BandSeq.band, band.id.get), By(BandSeq.seq, seq))
    bandSeqs match {
      case Nil => {
        band.bandSeqs += new BandSeq(new Date(0), new Date(0), seq)
        band
      }
      case _ => band
    }
  }

  private def generateBand2Seq(band: Band, seq: Int): BandSeq = {
    val bandSeqs = BandSeq.findAll(By(BandSeq.band, band.id.get), By(BandSeq.seq, seq))
    bandSeqs match {
      case Nil => {
        new BandSeq(new Date(0), new Date(0), seq)
      }
      case _ => bandSeqs.head
    }
  }

  private def duplicateAlbumCheck(albumid: Long, seq: Long):Boolean =
    Album.findAll(By(Album.id, albumid)).size.equals(0)

  private def duplicateKeyCheck(album: Target, band: Binder):Boolean =
    !band.getTargets.forall(bseq => bseq.asInstanceOf[BandSeq].getTarget2s.forall(alb => alb.albumtitle != album.getName))
}
