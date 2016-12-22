package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import Helpers._

import code.model.Album
import code.model.Band
import code.model.BandSeq
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
    var albumtitle = ""
    var artistname = ""
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
      val msg = "Can not register. Already exist Album. Please update"
      val path = "/"
      Logic.select(duplicateAlbumCheck, _==_ )(albumid, albumid, albumid, msg, path) match {
        case "add" => addAlbum()
        case "update" => println("update")
      }
      S.mapSnippet("AlbumView.add", doBind)
    }

    def addAlbum() = {
      val band: Band = generateBand(artistname)
      val regSeq = Util.isAllDigits(artistseq) match {
        case true => artistseq.toInt
        case _ => 1
      }
      val bandSeq = generateBandSeq(band, regSeq)
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
      var album = new Album(albumtitle)
      album.validate match{
        case Nil => ()
        case errors => S.error(errors); S.redirectTo("/")
      }
      if(band.bandSeqs.forall(bseq => bseq.albums.forall(alb => alb.albumtitle != albumtitle))) {
        band.bandSeqs += bandSeq
        band.save
        bandSeq.albums += album
        bandSeq.save();
        S.notice("Added " + album.albumtitle);
        S.redirectTo("/")
      } else {
        S.error("duplicate album"); S.redirectTo("/")
      }
    }

    def updateAlbum() = {
      S.mapSnippet("AlbumView.add", doBind)
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

  private def generateBand(artistName: String): Band = {
    val bands = Band.findAll(By(Band.bandname,artistName)) 
    bands match {
      case Nil => {
        new Band(artistName)
      }
      case _ => bands.head
    }
    
  }

  private def generateBandSeq(band: Band, seq: Int): BandSeq = {
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
}
