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
import code.model._
import code.logic._
import java.util.Date

class BandView {
  val bandid = Util.paramGet("bandid")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  var initSeq = Util.paramGet("seq")
  var seq = Util.generateSeq(band.bandSeqs.size,
              () => band.bandSeqs.reduceLeft((bs1, bs2) => if(bs1.seq.get > bs2.seq.get) bs1 else bs2).seq.get + 1,
              Util.paramGet("seq"))
  val bandSeq = Util.paramGet("seq") match {
                  case "0" => new BandSeq(new Date(0), new Date(0), seq.toInt)
                  case _   => band.bandSeqs.filter{ bs => bs.seq == seq.toInt }.head
                }
  var startat = Util.dateToString(bandSeq.bandSeqStartAt.get)
  var endat = Util.dateToString(bandSeq.bandSeqEndAt.get)

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
    val msg = "Can not register.Already exsist Band Seq. Please update"
    val path = "/band?bandid=" + bandid
    val process = Logic.select(duplicateSeqCheck, _==_ )(bandid.toLong, initSeq.toLong, initSeq.toLong, msg, path)
    process match {
      case "add" => {
        val msg = Process.add((f: (Target, Binder) => Boolean) => (Target, Relation, Binder, String) => Nil, (Target, Binder) => true, String => Nil, "", null, new BandSeq(Util.stringToDate(startat), Util.stringToDate(endat), seq.toInt).band(band.id.get), null, None, 0, "added seq " + seq, "")
        S.error(msg)
        S.redirectTo(path)
      }
      case "update" => {
        val msg = Process.update((f2: (Long, Long) => Target, f3: Long => Binder,  f4: String => List[Target]) => (a, b, c) => new Result(false, ""), (a, b) => band.bandSeqs.filter{ bs => bs.seq == seq.toLong }.head.bandSeqStartAt(Util.stringToDate(startat)).bandSeqEndAt(Util.stringToDate(endat)), a => null, c => Nil, upload => false, c => Nil, "", 0L, null, band, 0L, None, "updated seq " + seq, "", "")
        S.error(msg)
        S.redirectTo(path)
      }
    }
  }

  private 
    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
//      var bandseqs: List[BandSeq] = BandSeq.findAll(By(BandSeq.band, bandid.toLong), OrderBy(BandSeq.seq, Ascending))
      val seqSize = band.bandSeqs.size
      band.bandSeqs.flatMap(bds =>
        bds.seq.equals(seqSize) match {
          case true => relationNonExisti(findAlbum)(bds.id.get) && relationNonExisti(findBandSeqPlayers)(bds.id.get) match {
            case true => 
                          bind("band", html,
                          "seq" -> <span>{
                             link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString , () => (), Text(bds.seq.get.toString))
                          }</span>,
                          "startat" -> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span>,
                          "endat" -> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span>,
                          "delete" -> <span>{link("band?bandid=" + bandid, () => delete(bandid.toLong, bds.seq.get), Text("delete"))}</span>
                          )
            case _ => 
                          bind("band", html,
                          "seq" -> <span>{
                             link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString , () => (), Text(bds.seq.get.toString))
                          }</span>,
                          "startat" -> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span>,
                          "endat" -> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span>,
                          "delete" -> <span></span>
                          )
            }
          case _ => 
                          bind("band", html,
                          "seq" -> <span>{
                             link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString , () => (), Text(bds.seq.get.toString))
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
      bandSeq.players --= bandSeq.players
//      bandSeq.players.map{ p => bandSeq.players -= p }
      bandSeq.save
      bandSeq.delete_!
   }

   def relationNonExisti(findTable: Long => Boolean)(id: Long): Boolean = findTable(id)

   def findAlbum(id: Long): Boolean = Album.findAll(By(Album.bandseq, id)).size.equals(0)

   def findBandSeqPlayers(id: Long): Boolean = BandSeqPlayers.findAll(By(BandSeqPlayers.bandseq, id)).size.equals(0)

   def duplicateSeqCheck(bandid: Long, seq: Long): Boolean =
         Band.findAll(By(Band.id, bandid)).head.bandSeqs.filter{ bs => bs.seq == seq }.size.equals(0)
}
