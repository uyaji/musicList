package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import Helpers._

import code.model.Album
import code.model.Band
import code.model.BandSeq
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
    var albumtitle = ""
    var artistname = ""
    var artistseq = ""

    def addAlbum() = {
      val bands = Band.findAll(By(Band.bandname,artistname)) 
      val band = bands match {
        case Nil => {
          new Band(artistname)
        }
        case _ => bands.head
      }
      val bandSeqs = BandSeq.findAll(By(BandSeq.band, band.id.get), By(BandSeq.seq, artistseq.toInt))
      val bandSeq = bandSeqs match {
        case Nil => {
          val regSeq = artistseq.toInt match {
            case 0 => 1
            case n: Int => n
          }
          new BandSeq(new Date(0), new Date(0), regSeq.toInt)
        }
        case _ => bandSeqs.head
      }
      bandSeqs match {
        case Nil => {
          band.validate match {
            case Nil => {
              band.bandSeqs += bandSeq
              band.save
            }
            case errors => {
              S.error(errors)
              S.mapSnippet("AlbumView.add", doBind)
            }
          }
        }
        case _ => ()
      }
      bandSeq.validate match {
        case Nil => {
          var album = new Album(albumtitle)
          album.validate match{
            case Nil => {
              bandSeq.albums += album
              bandSeq.save();
              S.notice("Added " + album.albumtitle);
            }
            case errors => S.error(errors); S.mapSnippet("AlbumView.add", doBind)
          }
        }
        case errors => {
          S.error(errors)
          S.mapSnippet("AlbumView.add", doBind)
        }
      }
    }

    def doBind(from: NodeSeq): NodeSeq = {
      var sel =
        "name=albumtitle" #> SHtml.text(albumtitle, albumtitle = _) &
        "name=artistname" #> SHtml.text(artistname, artistname = _) &
        "name=artistseq"  #> SHtml.text(artistseq, artistseq = _) &
        "type=submit" #> SHtml.onSubmitUnit(addAlbum);
      return sel(from)
    }

    doBind(from)
  }

  
  private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
    var albums:List[Album] = Album.findAll(OrderBy(Album.albumtitle, Ascending))
    albums.flatMap(alb =>
      bind("album", html, AttrBindParam("id", alb.id.toString, "id"),
                          "albumtitle" -> <span>{link("track?albumid=" + alb.id.toString, () => (), Text(alb.albumtitle.get))}</span>,
                          "artistname" -> <span>{link("band?bandid="+ alb.getBandSeq().getBand().id.toString, () => (), Text(alb.getBandSeq().getBand().bandname.get))}</span>,
                          "artistseq" -> <span>{link("member?bandid="+ alb.getBandSeq().getBand().id.toString + "&seq=" + alb.getBandSeq().seq.toString, () => (), Text(alb.getBandSeq().seq.toString))}</span>
      )
    )
  }          

}
