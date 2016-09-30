package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import code.model.Band
import code.model.BandSeq
import java.text.SimpleDateFormat
import java.util.Date

class BandView {
  val bandid = getBandId()
  val band = Band.findAll(By(Band.id, bandid.toLong)).head

  def list(html: NodeSeq): NodeSeq = {
    def renderRow(): NodeSeq = {
      def reDraw() = JsCmds.Replace("bad_history", renderRow())
      var render = "#band_history * " #>((n: NodeSeq) => doList(reDraw)(n))
      render(html)
    }
    renderRow()
  }

  def bandName(html: NodeSeq): NodeSeq = {
    bind("band", html, "name" -> band.bandname)
  }
  
  def add(form: NodeSeq): NodeSeq = {
    var seq = ""
    var startat = ""
    var endat = ""

    def addSeq() = {
      band.bandSeqs += new BandSeq(stringToDate(startat), stringToDate(endat), seq.toInt)
      band.save
      S.notice("Added Seq " + seq)
      S.redirectTo("/band?bandid=" + bandid)
    }

    def doBind(form: NodeSeq): NodeSeq = {
      var sel =
        "name=seq" #> SHtml.onSubmit(seq = _) &
        "name=startat" #> SHtml.onSubmit(startat = _) &
        "name=endat" #> SHtml.onSubmit(endat = _) &
        "type=submit" #> SHtml.onSubmitUnit(addSeq);
      return sel(form)
    }
    doBind(form)
  }

  private 
    def getBandId(): String = {
      S.param("bandid") match {
        case Full(id) => id
        case _ => "0"
      }
    }

    def stringToDate(strDate: String): Date = {
      val sdf = new SimpleDateFormat("yyyy");
      sdf.parse(strDate.substring(0,4)) 
    }

    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      var bandseqs: List[BandSeq] = BandSeq.findAll(By(BandSeq.band, bandid.toLong), OrderBy(BandSeq.seq, Ascending))
      bandseqs.flatMap(bds =>
        bind("band", html,
                        "seq" -> <span>{bds.seq.get}</span>,
                        "startat" -> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span>,
                        "endat" -> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span>
        )
      )
    }
}
