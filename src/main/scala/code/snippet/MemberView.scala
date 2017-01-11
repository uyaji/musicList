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

class MemberView {
  val bandid = Util.paramGet("bandid")
  val bandseq = Util.paramGet("seq")
  val memberseq = Util.paramGet("memberseq")
  val bandseqplayerid = Util.paramGet("bandseqplayerid")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  val bandSeq = band.bandSeqs.filter{ bs => bs.seq == bandseq.toLong }.head
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
        "name=name" #> SHtml.text( name, name = _) &
        "type=submit" #> SHtml.onSubmitUnit(process);
      return sel(form)
    }
    doBind(form)
  }

  def process() {
    try {
      val errMsg = "Can not register.Already exsist member. Please update"
      val path = "/member?bandid=" + bandid + "&seq=" + bandseq
      val addMsg = "Added member " + name
      val updateMsg = "updated member " + name
      val errMsgMember = "Duplicate member!"
      Logic.select(duplicateSeqCheck, changeSeqCheck)(bandSeq.id.get, seq.toLong, bandseqplayerid.toLong, errMsg, path) match {
        case "add" => Process.add(Logic.registTarget, duplicateKeyCheck, getExistPlayer, name, bandSeq, Player.create.name(name), BandSeqPlayers.create.bandseq(bandSeq.id.get).seq(seq.toLong), None, 0, addMsg, errMsgMember, path)
        case "update" => updateMember(Logic.updateTarget, getPlayer, getBinder, getExistPlayer, name, seq.toLong, bandSeq, updateMsg, errMsgMember, path)
      }
    } catch {
      case e: java.lang.NumberFormatException => {
        S.error("SEQ must be the number!")
        S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
      }
    }
  }

  def updateMember(function1: ((Long, Long) => Target, Long => Binder, String => List[Target]) => (Long, Long, String) => Result, function2: (Long, Long) => Target, function3: Long => Binder, function4: String => List[Target], name: String, seq: Long, bandSeq: BandSeq, msg: String, errMsg: String, path: String) {
    val player = getPlayer(bandSeq.id.get, bandseqplayerid.toLong)
    val bandSeqPlayer = player.getRelation(bandseqplayerid.toLong)
    val existPlayers = getExistPlayer(name)
    val result = function1(function2, function3, function4)(bandSeq.id.get, bandseqplayerid.toLong, name)
    result.error match {
      case true => {
        S.error(errMsg)
        S.redirectTo(path)
      }
      case flase => ()
    }
    result.changeContent match {
      case "name" => {
        player.name(name)
      }
      case _ => {
        bandSeqPlayer.player(existPlayers.head.getId)
      }
    }
    bandSeqPlayer.seq(seq.toLong)
    bandSeqPlayer.save
    player.save
    S.notice(msg)
    S.redirectTo(path)
  }

  private
    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      bandSeq.players.flatMap(pl => {
        val bsp = pl.bandSeqPlayers.filter{bsp => bsp.bandseq.get.equals(bandSeq.id.get)}
        val memberseq = bsp.isEmpty match {
          case true => "0"
          case false => bsp.head.seq.toString
        }
        val bandseqplayerid: String = pl.bandSeqPlayers.filter{bsp => bsp.bandseq == bandSeq.id.get}.head.id.get.toString
        bind("member", html,
          "seq" -> <span>{
            link("member?bandid=" + bandid + "&seq=" + bandseq + "&memberseq=" + memberseq + "&bandseqplayerid=" + bandseqplayerid, () => (), Text(memberseq))
          }</span>,
          "name" -> <span>{pl.name}</span>,
          "delete" -> <span>{link("member?bandid=" + bandid + "&seq=" + bandseq, () => delete(bandSeq.id.get, pl.id.get), Text("delete"))}</span>
        )
      })
    }

    def delete(bandseqid: Long, playerid: Long): Unit = {
      val player = Player.findAll(By(Player.id, playerid)).head
      player.bandseqs.size match {
        case 1 => player.delete_!
        case _ => ()
      }
      val bandSeq = BandSeq.findAll(By(BandSeq.id, bandseqid)).head
      bandSeq.players -= player
      bandSeq.save
      S.notice("Deleted " + player.name)
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
