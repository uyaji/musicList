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

import java.net.URL
import scala.util.parsing.combinator.RegexParsers

class AlbumView {

  var searchAlbumtitle = Util.paramGet("searchAlbumtitle") match {
    case "0" => ""
    case key => key
  }
  var searchArtist = Util.paramGet("searchArtist") match {
    case "0" => ""
    case key => key
  }

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
    var band: Band = null
    var bandseq: BandSeq = null
    albumid match {
      case 0 => ()
      case aid => {
        val album = Album.findAll(By(Album.id, aid.toLong)).head
        albumtitle = album.albumtitle.get
        bandseq = BandSeq.findAll(By(BandSeq.id, album.bandseq.get)).head
        band = Band.findAll(By(Band.id, bandseq.band.get)).head
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
          val msg = Process.add(Logic.registTarget, duplicateKeyCheck, title => Nil, albumtitle, generateBand(artistname, regSeq), new Album(albumtitle).bandseq(generateBand(artistname, regSeq).bandSeqs.filter{bsq => bsq.seq==regSeq}.head.id.get), null, None, regSeq, "added " + albumtitle, errorMsg)
          S.error(msg)
        }
        case "update" => {
          val band = generateBand(artistname, regSeq)
          val msg = Process.update(Logic.updateTarget, (id, id2) => Album.findAll(By(Album.id, id2.toLong)).head, id => Band.findAll(By(Band.id, id.toLong)).head, key => Album.findAll(By(Album.albumtitle, key)), upload => false, name => Nil, albumtitle, regSeq, null, band, albumid, None, "updated " + albumtitle, errorMsg, "")
          S.error(msg)
        }
      }
      S.redirectTo(path)
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

  def search(from : NodeSeq): NodeSeq = {

    def doBind(from: NodeSeq): NodeSeq = {
      var sel =
        "name=searchAlbumtitle" #> SHtml.text(searchAlbumtitle, searchAlbumtitle = _) &
        "name=searchArtist" #> SHtml.text(searchArtist, searchArtist = _) &
        "type=submit" #> SHtml.onSubmitUnit(searchAlbum);
      return sel(from)
    }

    def searchAlbum() {
      S.redirectTo("/?searchAlbumtitle=" + urlEncode(searchAlbumtitle) + "&searchArtist=" + urlEncode(searchArtist))
    }

    doBind(from)
  }

  
  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    import scala.math.Ordering.Implicits._
    val albums: List[Album] = searchAlbumtitle match {
      case "" => {
        searchArtist match {
          case "" => Nil
          case artist: String => Band.findAll(Like(Band.bandname, "%" + searchArtist + "%")).flatMap { bd => bd.getBandSeq}.flatMap { bsq => bsq.getAlbum}.sorted
        }
      }
      case title: String => {
        searchArtist match {
          case "" => Album.findAll(Like(Album.albumtitle, "%" +searchAlbumtitle + "%"), OrderBy(Album.albumtitle, Ascending))
          case artist: String => Album.findAll(Like(Album.albumtitle, searchAlbumtitle + "%"), OrderBy(Album.albumtitle, Ascending)).withFilter(alb => (alb.getBandSeq.getBand.bandname.get.toUpperCase indexOf artist.toUpperCase) >= 0).map(alb => alb)
        }
      }
    }
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

  private def duplicateAlbumCheck(albumid: Long, seq: Long):Boolean =
    Album.findAll(By(Album.id, albumid)).size.equals(0)

  private def duplicateKeyCheck(album: Target, band: Binder):Boolean =
    !band.getTargets.forall(bseq => bseq.asInstanceOf[BandSeq].getTarget2s.forall(alb => alb.albumtitle != album.getName))
}
