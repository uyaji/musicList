package code.snippet
import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http.js.{JsCmd, JsCmds}
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import code.model.Band
import code.model.BandSeq
import code.model.Player
import code.model.BandSeqPlayers

class MemberView {
  val bandid = Param.get("bandid")
  val bandseq = Param.get("seq")
  val memberseq = Param.get("memberseq")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  val bandSeq = band.bandSeqs.filter{ bs => bs.seq == bandseq.toLong }.head
  var name = ""
  var seq = Generater.generateSeq(bandSeq.bandseqPlayers.size,
              () => bandSeq.bandseqPlayers.reduceLeft((bp1, bp2) => if(bp1.seq.get > bp2.seq.get) bp1 else bp2).seq.get +1,
              memberseq)
  def bandNameSeq(html: NodeSeq): NodeSeq = {
    bind("band", html, "nameseq" -> (band.bandname + " : " + bandseq ))
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
        "name=seq" #> SHtml.text( seq, seq = _) &
        "name=name" #> SHtml.text( name, name = _) &
        "type=submit" #> SHtml.onSubmitUnit(registerMember);
      return sel(form)
    }
    doBind(form)
  }

  def registerMember() {
    val player: Player = Player.findAll(By(Player.name, name)) match {
                   case Nil => {
                     new Player(name)
                   }
                   case pl: List[Player] => pl.head
    }
    player.save
    val bandSeqPlayer: BandSeqPlayers = BandSeqPlayers.create.bandseq(bandSeq.id.get).player(player.id.get).seq(seq.toLong)
    bandSeqPlayer.save
    S.notice("Added member " + player.name)
    S.redirectTo("/member?bandid=" + bandid + "&seq=" +bandseq)
  }
  
  private
    def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
      bandSeq.players.flatMap(pl =>
        bind("member", html,
          "seq" -> <span>{pl.bandSeqPlayers.filter{bsp => bsp.bandseq.get.equals(bandSeq.id.get)}.head.seq}</span>,
          "name" -> <span>{pl.name}</span>,
          "delete" -> <span>delete</span>
        )
      )
    }
}
