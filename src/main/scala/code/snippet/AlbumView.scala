package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import Helpers._

import code.model._
import code.logic._
import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import java.util.Date

import java.net.URL
import scala.util.parsing.combinator.RegexParsers

class AlbumView extends PaginatorSnippet[Album]{
  val offset = Util.paramGet("offset")
  var searchAlbumtitle = Util.paramGet("searchAlbumtitle") match {
    case "0" => ""
    case key => key
  }
  var searchArtist = Util.paramGet("searchArtist") match {
    case "0" => ""
    case key => key
  }
  var searchTrack = Util.paramGet("searchTrack") match {
    case "0" => ""
    case key => key
  }
  var searchPlayer = Util.paramGet("searchPlayer") match {
    case "0" => ""
    case key => key
  }

  override val itemsPerPage = 5
  override def page = genList
  override def count = page.size
  override def pageUrl(offset: Long): String = appendParams(super.pageUrl(offset), List("searchAlbumtitle" -> searchAlbumtitle, "searchArtist" -> searchArtist, "searchTrack" -> searchTrack, "searchPlayer" -> searchPlayer))

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
      artistseq = Util.paramGet("artistseq") match {
        case "0" => artistseq
        case _ => Util.paramGet("artistseq")
      }
      val errorMsg = "Can not register. Already exist Album. Please update"
      var path = "/?searchAlbumtitle=" + urlEncode(searchAlbumtitle) + "&searchArtist=" + urlEncode(searchArtist) + "&searchTrack=" + urlEncode(searchTrack) + "&searchPlayer=" + urlEncode(searchPlayer) + "&offset=" + offset
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
      path = (searchAlbumtitle, searchArtist, searchTrack, searchPlayer) match {
        case ("", "", "", "") => "/?searchAlbumtitle=" + urlEncode(albumtitle) + "&searchArtist=" + urlEncode(artistname) + "&searchTrack=" + urlEncode(searchTrack) + "&searchPlayer=" + urlEncode(searchPlayer) + "&offset=" + offset
        case _ => path
      }
      S.redirectTo(path)
    }

    def doBind(from: NodeSeq): NodeSeq = {
      val options: List[(String, String)] = band match {
        case null => List(("",""))
        case _ => band.bandSeqs.map { bs => (bs.seq.get.toString, bs.seq.get.toString) }.toList
      }
      val default = band match {
        case null => Empty
        case _ => Full(bandseq.seq.get.toString)
      }
      var sel = bandseq match {
        case null => {
          "name=albumtitle" #> SHtml.text(albumtitle, albumtitle = _) &
          "name=artistname" #> SHtml.text(artistname, artistname = _, "class" -> "search") &
          "name=artistseq"  #> SHtml.text(artistseq, artistseq = _) &
          "type=submit" #> SHtml.onSubmitUnit(albumProcess);
        }
        case _ => {
          "name=albumtitle" #> SHtml.text(albumtitle, albumtitle = _) &
          "name=artistname" #> SHtml.text(artistname, artistname = _, "class" -> "search") &
          "name=artistseq"  #> SHtml.select(options, default, artistseq = _) &
          "type=submit" #> SHtml.onSubmitUnit(albumProcess);
        }
      }
      return sel(from)
    }

    doBind(from)
  }

  def search(from : NodeSeq): NodeSeq = {

    def doBind(from: NodeSeq): NodeSeq = {
      var sel =
        "name=searchAlbumtitle" #> SHtml.text(searchAlbumtitle, searchAlbumtitle = _, "class" -> "search") &
        "name=searchArtist" #> SHtml.text(searchArtist, searchArtist = _, "class" -> "search") &
        "name=searchTrack" #> SHtml.text(searchTrack, searchTrack = _, "class" -> "search") &
        "name=searchPlayer" #> SHtml.text(searchPlayer, searchPlayer = _, "class" -> "search") &
        "type=submit" #> SHtml.onSubmitUnit(searchAlbum);
      return sel(from)
    }

    def searchAlbum() {
      S.redirectTo("/?searchAlbumtitle=" + urlEncode(searchAlbumtitle)
      + "&searchArtist=" + urlEncode(searchArtist) + "&searchTrack=" + urlEncode(searchTrack) + "&searchPlayer=" + urlEncode(searchPlayer)
      )
    }

    doBind(from)
  }

  
  private def genList = {
    val albumsFilterTitle: List[Album] = searchAlbumtitle match {
      case "" => Nil
      case title: String => Album.findAll(Like(Album.albumtitle, "%" +searchAlbumtitle + "%"), OrderBy(Album.albumtitle, Ascending))
      case _ => Nil
    }
    val albumsFilterArtist: List[Album] = searchArtist match {
      case "" => albumsFilterTitle
      case artist: String => {
        albumsFilterTitle match {
          case Nil => Band.findAll(Like(Band.bandname, "%" + searchArtist + "%")).flatMap { bd => bd.getBandSeq}.flatMap { bsq => bsq.getAlbum}
          case albums: List[Album] => albums.withFilter(alb => (alb.getBandSeq.getBand.bandname.get.toUpperCase indexOf artist.replace("%","").toUpperCase) >= 0).map(alb => alb)
          case _ => Nil
        }
      }
      case _ => albumsFilterTitle
    }
    val albumsFilterTrack: List[Album] = searchTrack match {
      case "" => albumsFilterArtist.map(alb => alb)
      case track: String => {
        albumsFilterArtist match {
          case Nil => Track.findAll(Like(Track.tracktitle, "%" + searchTrack + "%")).flatMap { trc => trc.albums }
          case albums: List[Album] => albums.withFilter(alb => (alb.tracks.withFilter(trc => (trc.tracktitle.get.toUpperCase indexOf track.replace("%","").toUpperCase) >= 0).map(trc => trc).size > 0)).map(alb => alb)
          case _ => Nil
        }
      }
      case _ => albumsFilterArtist
    }
    val albums = searchPlayer match {
      case "" => albumsFilterTrack
      case player: String => {
        albumsFilterTrack match {
          case Nil => Player.findAll(Like(Player.name, "%" + searchPlayer + "%")).flatMap { pl => pl.bandseqs}.flatMap { bsq => bsq.getAlbum}
          case albums: List[Album] => albums.withFilter(alb => (alb.getBandSeq.players.withFilter(pl => (pl.name.get.toUpperCase indexOf player.replace("%", "").toUpperCase) >= 0).map(trc => trc).size > 0)).map(alb => alb)
          case _ => Nil
        }
      }
      case _ => Nil
    }
    albums.distinct
  }

  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    var seq: Int = offset.toInt
    page.toList.sorted.drop(curPage * itemsPerPage).take(itemsPerPage).flatMap(alb => {
      seq = seq + 1 
      bind ("album", html, AttrBindParam("id", alb.id.toString, "id"),
                          "seq" -> <span>{link("/?albumid=" + alb.id.toString + "&searchAlbumtitle=" + urlEncode(searchAlbumtitle) + "&searchArtist=" + urlEncode(searchArtist) + "&searchTrack=" + urlEncode(searchTrack) + "&searchPlayer=" + urlEncode(searchPlayer) + "&offset=" + offset, () => (), Text(seq.toString))}</span>,
                          "albumtitle" -> <span>{link("track?albumid=" + alb.id.toString, () => (), Text(alb.albumtitle.get))}</span>,
                          "artistname" -> <span>{link("band?bandid="+ alb.getBandSeq().getBand().id.toString, () => (), Text(alb.getBandSeq().getBand().bandname.get))}</span>,
                          "artistseq" -> <span>{link("member?bandid="+ alb.getBandSeq().getBand().id.toString + "&seq=" + alb.getBandSeq().seq.toString, () => (), Text(alb.getBandSeq().seq.toString))}</span>,
                          "tracktitle" -> <span>{Text(if(searchTrack.equals("")) "" else alb.tracks.withFilter(trc => (trc.tracktitle.get.toUpperCase indexOf searchTrack.replace("%","").toUpperCase) >= 0).map(trc =>trc).toList.head.tracktitle.get)}</span>,
                          "tracktitles" -> <span class="tracktitles">{Text(if(searchTrack.equals("")) "" else {if(alb.tracks.withFilter(trc => (trc.tracktitle.get.toUpperCase indexOf searchTrack.replace("%","").toUpperCase) >= 0).map(trc =>trc).toList.size>1) "+" else ""})}</span>,
                          "player" -> <span>{Text(if(searchPlayer.equals("")) "" else alb.getBandSeq.players.withFilter(pl => (pl.name.get.toUpperCase indexOf searchPlayer.replace("%","").toUpperCase) >= 0).map(pl =>pl).toList.head.name.get)}</span>,
                          "players" -> <span class="players" width="1">{Text(if(searchPlayer.equals("")) "" else { if(alb.getBandSeq.players.withFilter(pl => (pl.name.get.toUpperCase indexOf searchPlayer.replace("%","").toUpperCase) >= 0).map(pl => pl).toList.size>1) "+" else ""})}</span>
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

