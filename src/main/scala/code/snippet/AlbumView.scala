package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import Helpers._

import code.model.Album
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

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

    def addAlbum() = {
      var album = new Album(albumtitle, artistname)
      album.validate match{
        case Nil => {
          album.save(); S.notice("Added " + album.albumtitle);
        }
        case x => S.error(x); S.mapSnippet("AlbumView.add", doBind)
      }
    }

    def doBind(from: NodeSeq): NodeSeq = {
      var sel =
        "name=albumtitle" #> SHtml.onSubmit(albumtitle = _) &
        "name=artistname" #> SHtml.onSubmit(artistname = _) &
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
                          "artistname" -> <span>{alb.artistname.get}</span>
      )
    )
  }          

}
