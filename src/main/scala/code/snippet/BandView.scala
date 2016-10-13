//ToDo 最終行のみdelete action表示すること 2016/10/07
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
  val bandid = Param.get("bandid")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  var initSeq = Param.get("seq")
  var seq = Param.get("seq") match {
              case "0" => band.bandSeqs.size match {
                  case 0 => "1"
                  case _ => (band.bandSeqs.reduceLeft((bs1, bs2) => if(bs1.seq.get > bs2.seq.get) bs1 else bs2).seq.get + 1).toString
              }
              case _  => Param.get("seq")
            }
  val bandSeq = Param.get("seq") match {
                  case "0" => new BandSeq(new Date(0), new Date(0), seq.toInt)
                  case _   => band.bandSeqs.filter{ bs => bs.seq == seq.toInt }.head
                }
  var startat = dateToString(bandSeq.bandSeqStartAt.get)
  var endat = dateToString(bandSeq.bandSeqEndAt.get)

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
    def doBind(form: NodeSeq): NodeSeq = {
      var sel =
        "name=seq" #> SHtml.text( seq, seq = _, "readonly" -> "readonly") &
        "name=startat" #> SHtml.text( startat, startat = _) &
        "name=endat" #> SHtml.text( endat, endat = _) &
        "type=submit" #> SHtml.onSubmitUnit(registerSeq);
      return sel(form)
    }
    doBind(form)
  }

  def registerSeq() {
    Process.select(duplicateSeqCheck, _==_ )(bandid.toLong, initSeq.toLong, initSeq.toLong) match {
      case "add" => addSeq
      case "update" => updateSeq
    }
  }

  def addSeq() {
    band.bandSeqs += new BandSeq(stringToDate(startat), stringToDate(endat), seq.toInt)
    band.save
    S.notice("Added Seq " + seq)
    S.redirectTo("/band?bandid=" + bandid)
  }

  def updateSeq() {
    var bandSeq = band.bandSeqs.filter{ bs => bs.seq == seq.toInt }.head
    bandSeq.bandSeqStartAt(stringToDate(startat))
    bandSeq.bandSeqEndAt(stringToDate(endat))
    bandSeq.save
    S.notice("Updated Seq " + seq)
    S.redirectTo("/band?bandid=" + bandid)
  }

  private 
    def stringToDate(strDate: String): Date = {
      val sdf = new SimpleDateFormat("yyyy");
      sdf.parse(strDate.substring(0,4)) 
    }

    def dateToString(date: Date): String = {
      val sdf = new SimpleDateFormat("yyyy");
      sdf.format(date) 
    }

    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      var bandseqs: List[BandSeq] = BandSeq.findAll(By(BandSeq.band, bandid.toLong), OrderBy(BandSeq.seq, Ascending))
      val seqSize = bandseqs.size
      bandseqs.flatMap(bds =>
        bds.seq.equals(seqSize) match {
          case true =>
                          bind("band", html,
                          "seq" -> <span>{
                             link("band?bandid=" + Param.get("bandid") + "&seq=" + bds.seq.toString , () => (), Text(bds.seq.get.toString))
                          }</span>,
                          "startat" -> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span>,
                          "endat" -> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span>,
                          "delete" -> <span>{link("band?bandid=" + bandid, () => delete(bandid.toLong, bds.seq.get), Text("delete"))}</span>
                          )
          case _ => 
                          bind("band", html,
                          "seq" -> <span>{
                             link("band?bandid=" + Param.get("bandid") + "&seq=" + bds.seq.toString , () => (), Text(bds.seq.get.toString))
                          }</span>,
                          "startat" -> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span>,
                          "endat" -> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span>,
                          "delete" -> <span></span>
                          )
        }
      )
    }
    
    def delete(bandid: Long, seq: Long): Unit = {
      val bandSeq = BandSeq.findAll(By(BandSeq.band, bandid), By(BandSeq.seq, seq.toInt)).head
      bandSeq.delete_!
   }

   def duplicateSeqCheck(bandid: Long, seq: Long): Boolean =
         Band.findAll(By(Band.id, bandid)).head.bandSeqs.filter{ bs => bs.seq == seq }.size.equals(0)

}
