package code.snippet
import scala.xml.{NodeSeq, Text}
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http.js.{JsCmd, JsCmds}
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import code.model._
import code.logic._

class MemberView extends PaginatorSnippet[BandSeqPlayers] {
  val bandid = Util.paramGet("bandid")
  val bandseq = Util.paramGet("seq")
  val offset = Util.paramGet("offset")
  val memberseq = Util.paramGet("memberseq")
  val bandseqplayerid = Util.paramGet("bandseqplayerid")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  val bandSeq = BandSeq.findAll(By(BandSeq.band, bandid.toLong), By(BandSeq.seq, bandseq.toInt)).head

  override val itemsPerPage = 5
  override def pageUrl(offset: Long): String = appendParams( super.pageUrl(offset), List("bandid" -> bandid, "seq" -> bandseq))
  override def count = BandSeqPlayers.findAll(By(BandSeqPlayers.bandseq, bandSeq.id.get)).size
  override def page = BandSeqPlayers.findAll(By(BandSeqPlayers.bandseq, bandSeq.id.get), OrderBy(BandSeqPlayers.seq, Ascending), StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

  var name: String = memberseq match {
               case "0" => ""
               case _   => bandSeq.bandseqPlayers.filter{bsp => bsp.seq == memberseq.toLong}.head.getPlayer.name.get
             }
  var seq = Util.generateSeq(bandSeq.bandseqPlayers.size,
              () => bandSeq.bandseqPlayers.reduceLeft((bp1, bp2) => if(bp1.seq.get > bp2.seq.get) bp1 else bp2).seq.get +1,
              memberseq)
  def bandNameSeq(html: NodeSeq): NodeSeq = {
    bind("band", html, "nameseq" -> (band.bandname + " : " + Util.dateToString(bandSeq.bandSeqStartAt.get) + " - " + Util.dateToString(bandSeq.bandSeqEndAt.get) ))
  }

  def list(html: NodeSeq): NodeSeq = {
    def renderRow(): NodeSeq = {
      def reDraw() = JsCmds.Replace("members", renderRow())
      var render = "#members * " #> ((n: NodeSeq) => doList(reDraw)(n))
      render(html)
    }
    renderRow()
  }

  def add(form: NodeSeq): NodeSeq = {
    def doBind(form: NodeSeq): NodeSeq = {
      var sel =
        "name=memberseq" #> SHtml.hidden( () => memberseq) &
        "name=bandseqplayerid" #> SHtml.hidden( () => bandseqplayerid) &
        "name=seq" #> SHtml.text( seq, seq = _) &
        "name=name" #> SHtml.text( name, name = _, "class" -> "search column span-10") &
        "type=submit" #> SHtml.onSubmitUnit(process);
      return sel(form)
    }
    doBind(form)
  }

  def process() {
    try {
      val errMsg = "Can not register.Already exsist member. Please update"
      val path = "/member?bandid=" + bandid + "&seq=" + bandseq + "&offset=" +offset
      val addMsg = "Added member " + name
      val updateMsg = "updated member " + name
      val errMsgMember = "Duplicate member!"
      Logic.select(duplicateSeqCheck, changeSeqCheck)(bandSeq.id.get, seq.toLong, bandseqplayerid.toLong, errMsg, path) match {
        case "add" => {
          val msg = Process.add(Logic.registTarget, duplicateKeyCheck, getExistPlayer, name, bandSeq, Player.create.name(name), BandSeqPlayers.create.bandseq(bandSeq.id.get).seq(seq.toLong), None, 0, addMsg, errMsgMember)
          S.error(msg)
        }
        case "update" => {
          val msg = Process.update(Logic.updateTarget, getPlayer, getBinder, getExistPlayer, upload => false, name => Nil, name, seq.toLong, null, bandSeq, bandseqplayerid.toLong, None, updateMsg, errMsgMember, "")
          S.error(msg)
        }
      }
      S.redirectTo(path)
    } catch {
      case e: java.lang.NumberFormatException => {
        S.error("SEQ must be the number!")
        S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
      }
    }
  }

  private
    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      def prevXml: NodeSeq = Text(?("<"))
      def nextXml: NodeSeq = Text(?(">"))
      def firstXml: NodeSeq = Text(?("<<"))
      def lastXml: NodeSeq = Text(?(">>"))
      def currentXml: NodeSeq = Text("Displaying records " + (first+1) + "-" + (first+itemsPerPage min count) + " of  " + count)
      page.flatMap(bsp => {
        val memberseq = bsp match {
          case null => "0"
          case _ => bsp.seq.toString
        }
        val bandseqplayerid: String = bsp.id.get.toString
        bind("member", html,
          "seq" -> <span>{
            link("member?bandid=" + bandid + "&seq=" + bandseq + "&memberseq=" + memberseq + "&bandseqplayerid=" + bandseqplayerid + "&offset=" + offset, () => (), Text(memberseq))
          }</span>,
          "name" -> <span>{Player.findAll(By(Player.id, bsp.player.get)).head.name}</span>,
          "delete" -> <span>{link("member?bandid=" + bandid + "&seq=" + bandseq + "&offset=" + offset, () => delete(bandSeq.id.get, bsp.player.get), Text("delete"))}</span>
        )
      })
    }

    def delete(bandseqid: Long, playerid: Long): Unit = {
      if(Util.isSuperUser) {
        val player = Player.findAll(By(Player.id, playerid)).head
        player.bandseqs.size match {
          case 1 => player.delete_!
          case _ => ()
        }
        val bandSeq = BandSeq.findAll(By(BandSeq.id, bandseqid)).head
        bandSeq.players -= player
        bandSeq.save
        S.notice("Deleted " + player.name)
      } else {
        S.error("You are not the super user.")
      }
    }

    def duplicateSeqCheck(bandseqid: Long, seq: Long): Boolean = {
      BandSeq.findAll(By(BandSeq.id, bandseqid)).head.bandseqPlayers.filter{ bsp => bsp.seq == seq }.size.equals(0)
    }
    def changeSeqCheck(bandseqPlayersId: Long, seq: Long): Boolean = {
      BandSeqPlayers.findAll(By(BandSeqPlayers.id, bandseqPlayersId)).head.seq.equals(seq)
    }
    def getBinder(bandseqid: Long): Binder = BandSeq.findAll(By(BandSeq.id, bandseqid)).head
    def getPlayer(bandseqid: Long, banseqplayrid: Long): Player =   BandSeq.findAll(By(BandSeq.id, bandseqid)).head.bandseqPlayers.filter{ bsp => bsp.id.get.toLong == bandseqplayerid.toLong}.head.getPlayer
    def getExistPlayer(name: String): List[Player] = Player.findAll(By(Player.name, name))
    def duplicateKeyCheck(player: Target, bandSeq: Binder): Boolean = BandSeq.findAll(By(BandSeq.id, bandSeq.getId)).head.getTargets.contains(player)
}
