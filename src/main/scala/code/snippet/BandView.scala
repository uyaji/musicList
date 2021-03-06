package code.snippet
import scala.xml.{NodeSeq, Text}
import net.liftweb._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.Run
import code.model._
import code.logic._
import java.util.Date
import java.text.SimpleDateFormat

class BandView extends PaginatorSnippet[BandSeq] {
  val bandid = Util.paramGet("bandid")
  val offset = Util.paramGet("offset")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  var initSeq = Util.paramGet("seq")
  var seq = Util.generateSeq(band.bandSeqs.size,
              () => band.bandSeqs.reduceLeft((bs1, bs2) => if(bs1.seq.get > bs2.seq.get) bs1 else bs2).seq.get + 1,
              Util.paramGet("seq"))
  val bandSeq = Util.paramGet("seq") match {
                  case "0" => new BandSeq(new Date(0), new Date(0), seq.toInt)
                  case _   => band.bandSeqs.filter{ bs => bs.seq == seq.toInt }.head
                }
  var startat = bandSeq.bandSeqStartAt.get
  var endat = bandSeq.bandSeqEndAt.get

  override val itemsPerPage = 5
  override def pageUrl(offset: Long): String = appendParams( super.pageUrl(offset), List("bandid" -> bandid))
  override def count = BandSeq.findAll(By(BandSeq.band, bandid.toLong)).size
  override def page = BandSeq.findAll(By(BandSeq.band, bandid.toLong), OrderBy(BandSeq.seq, Ascending), StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

  def list(html: NodeSeq): NodeSeq = {
    def renderRow(): NodeSeq = {
      def reDraw() = JsCmds.Replace("band_history", renderRow())
      var render = "#band_history * " #>((n: NodeSeq) => doList(reDraw)(n))
      render(html)
    }
    renderRow()
  }

  def bandName(html: NodeSeq): NodeSeq = {
    val bandBandname = "#bandbandname" #> band.bandname
    bandBandname(html)
  }
  
  def add(form: NodeSeq): NodeSeq = {
    def doBind(form: NodeSeq): NodeSeq = {
      val defaultst = (dateFormat format new Date(0))
      S.appendJs(enhance)
      var sel =
        "name=seq" #> SHtml.text( seq, seq = _, "readonly" -> "readonly") &
        "name=startat" #> SHtml.text( dateFormat format startat, logDateStart) &
        "name=endat" #> SHtml.text( dateFormat format endat, logDateEnd) &
        "type=submit" #> SHtml.onSubmitUnit(registerSeq);
      return sel(form)
    }
    doBind(form)
  }
  
  def logDateStart(s: String): Unit = {
    startat = tryo(dateFormat parse s) getOrElse new Date(0)
  }
  def logDateEnd(s: String): Unit = {
    endat = tryo(dateFormat parse s) getOrElse new Date(0)
  }
  
  val dateFormat = new SimpleDateFormat("yyyy")

  val enhance = Run("$('#startat').datepicker({dateFormat: 'yy', changeYear: true}); $('#endat').datepicker({dateFormat: 'yy', changeYear: true});")

  def registerSeq() {
    val msg = "Can not register.Already exsist Band Seq. Please update"
    val path = "/band?bandid=" + bandid + "&offset=" + offset
    val process = Logic.select(duplicateSeqCheck, _==_ )(bandid.toLong, initSeq.toLong, initSeq.toLong, msg, path)
    process match {
      case "add" => {
        val msg = Process.add((f: (Target, Binder) => Boolean) => (Target, Relation, Binder, String) => Nil, (Target, Binder) => true, String => Nil, "", null, new BandSeq(startat, endat, seq.toInt).band(band.id.get), null, None, 0, "added seq " + seq, "")
        S.error(msg)
      }
      case "update" => {
        val msg = Process.update((f2: (Long, Long) => Target, f3: Long => Binder,  f4: String => List[Target]) => (a, b, c, d) => new Result(Nil, ""), (a, b) => band.bandSeqs.filter{ bs => bs.seq == seq.toLong }.head.bandSeqStartAt(startat).bandSeqEndAt(endat), a => null, c => Nil, upload => false, c => Nil, "", 0L, null, band, 0L, None, "updated seq " + seq, "", "")
        S.error(msg)
      }
    }
    S.redirectTo(path)
  }

  private 
    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      def prevXml: NodeSeq = Text(?("<"))
      def nextXml: NodeSeq = Text(?(">"))
      def firstXml: NodeSeq = Text(?("<<"))
      def lastXml: NodeSeq = Text(?(">>"))
      def currentXml: NodeSeq = Text("Displaying records " + (first+1) + "-" + (first+itemsPerPage min count) + " of  " + count)
      val seqSize = band.bandSeqs.size
      page.flatMap(bds =>
        bds.seq.equals(seqSize) match {
          case true => relationNonExisti(findAlbum)(bds.id.get) && relationNonExisti(findBandSeqPlayers)(bds.id.get) match {
            case true => 
                          val bandseq = {
                            "#bandseq" #> <span>{
                             link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString + "&offset=" + offset , () => (), Text(bds.seq.get.toString))
                          }</span> &
                            "#bandstartat" #> <span>{link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))}</span> &
                            "#bandendat" #> <span>{bds.bandSeqEndAt.get.toString.substring(0, 4)}</span> &
                            "#banddelete" #> <span>{link("band?bandid=" + Util.paramGet("bandid") + "&offset=" + offset , () => delete(bandid.toLong, bds.seq.get), Text(S.loc("delete").head.toString))}</span>
                          }
                          bandseq(html)
            case _ => 
                          val bandseq = {
                            "#bandseq" #> <span>{
                               link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString + "&offset=" + offset , () => (), Text(bds.seq.get.toString))
                            }</span> &
                            "#bandstartat" #> <span>{
                              link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))
                            }</span> &
                            "#bandendat" #> <span>{
                              bds.bandSeqEndAt.get.toString.substring(0, 4)
                            }</span> &
                            "#banddelete" #> <span></span>
                          }
                          bandseq(html)
            }
          case _ => 
                          val bandseq = {
                            "#bandseq" #> <span>{
                               link("band?bandid=" + Util.paramGet("bandid") + "&seq=" + bds.seq.toString + "&offset=" + offset , () => (), Text(bds.seq.get.toString))
                            }</span> &
                            "#bandstartat" #> <span>{
                              link("member?bandid=" + bds.band.toString + "&seq=" + bds.seq.toString, () => (), Text(bds.bandSeqStartAt.get.toString.substring(0,4)))
                            }</span> &
                            "#bandendat" #> <span>{
                              bds.bandSeqEndAt.get.toString.substring(0, 4)
                            }</span> &
                            "#banddelete" #> <span></span>
                          }
                          bandseq(html)
        }
      )
    }
    
    def delete(bandid: Long, seq: Long): Unit = {
      if(Util.isSuperUser) {
        val bandSeq = BandSeq.findAll(By(BandSeq.band, bandid), By(BandSeq.seq, seq.toInt)).head
        bandSeq.players --= bandSeq.players
        bandSeq.save
        bandSeq.delete_!
     } else {
        S.error("You are not the super user.")
     }
   }

   def relationNonExisti(findTable: Long => Boolean)(id: Long): Boolean = findTable(id)

   def findAlbum(id: Long): Boolean = Album.findAll(By(Album.bandseq, id)).size.equals(0)

   def findBandSeqPlayers(id: Long): Boolean = BandSeqPlayers.findAll(By(BandSeqPlayers.bandseq, id)).size.equals(0)

   def duplicateSeqCheck(bandid: Long, seq: Long): Boolean =
         Band.findAll(By(Band.id, bandid)).head.bandSeqs.filter{ bs => bs.seq == seq }.size.equals(0)
}
