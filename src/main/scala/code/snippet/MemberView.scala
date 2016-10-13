package code.snippet
import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import code.model.Band
import code.model.Player
import code.model.BandSeqPlayers

class MemberView {
  val bandid = getParam("bandid")
  val bandseq = getParam("seq")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  var name = ""
  var seq = ""

  def bandNameSeq(html: NodeSeq): NodeSeq = {
    bind("band", html, "nameseq" -> (band.bandname + " : " + bandseq ))
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
    val bandSeq = band.bandSeqs.filter{ bs => bs.seq == bandseq.toLong }.head
    val bandSeqPlayer: BandSeqPlayers = BandSeqPlayers.create.bandseq(bandSeq.id.get).player(player.id.get).seq(seq.toLong)
    bandSeqPlayer.save
    S.notice("Added member " + player.name)
    S.redirectTo("/member?bandid=" + bandid + "&seq=" +bandseq)
  }

  private
    def getParam(key: String): String = {
      S.param(key) match {
        case Full(value) => value
        case _ => "none"
      }
    }
}
