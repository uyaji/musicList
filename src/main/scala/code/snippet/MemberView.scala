//AnyRefのno such methodのエラーは、targetとparentのsuper classを作成する。
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

class MemberView {
  val bandid = Param.get("bandid")
  val bandseq = Param.get("seq")
  val memberseq = Param.get("memberseq")
  val bandseqplayerid = Param.get("bandseqplayerid")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head
  val bandSeq = band.bandSeqs.filter{ bs => bs.seq == bandseq.toLong }.head
  var name: String = memberseq match {
               case "0" => ""
               case _   => bandSeq.bandseqPlayers.filter{bsp => bsp.seq == memberseq.toLong}.head.getPlayer.name.get
             }
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
      val msg = "Can not register.Already exsist member. Please update"
      val path = "/member?bandid=" + bandid + "&seq=" + bandseq
      Process.select(duplicateSeqCheck, changeSeqCheck)(bandSeq.id.get, seq.toLong, bandseqplayerid.toLong, msg, path) match {
        case "add" => registerMember()
        case "update" => updateMember
      }
    } catch {
      case e: java.lang.NumberFormatException => {
        S.error("SEQ must be the number!")
        S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
      }
    }
  }

  def registerMember() {
    val player: Player = Player.findAll(By(Player.name, name)) match {
                   case Nil => {
                     new Player(name)
                   }
                   case pl: List[Player] => pl.head
    }
    player.validate match {
      case Nil => {
        // 登録時にメンバーの重複がないかチェック
        bandSeq.players.toList.contains(player) match {
          case true => {
            S.notice("Duplicate member!")
            S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
          }
          case false => {
            player.save
            val bandSeqPlayer: BandSeqPlayers = BandSeqPlayers.create.bandseq(bandSeq.id.get).player(player.id.get).seq(seq.toLong)
            bandSeqPlayer.validate match {
              case Nil => {
                bandSeqPlayer.save
                S.notice("Added member " + player.name)
                S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
              }
              case errors => {
                S.error(errors)
                S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
              }
            }
          }
        }
      }
      case errors => {
        S.error(errors)
        S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
      }
    }
  }

  def updateMember {
    val player = bandSeq.bandseqPlayers.filter{ bsp => bsp.id == bandseqplayerid.toLong}.head.getPlayer
    val bandseqplayer = BandSeqPlayers.findAll(By(BandSeqPlayers.id, bandseqplayerid.toLong)).head
    // 指定されたPlayerが既存かどうかチェック。
    //   既存: BandSeqPlayersのアソシエーションの変更
    //   未存: Playerのnameの更新
    var exist = false
    // 入力されたnameで、playerオブジェクトをインスタンス化
    val existPlayers = Player.findAll(By(Player.name, name))
    // nameの変更を確認
    player.name.equals(name) match {
      // 変更が無ければ、重複問題なし
      case true => exist = true
      // 変更の場合、重複の可能性あり
      case _ => {
        // 入力playerオブジェクトの有無確認
        existPlayers.size match {
          case 0 => exist = true
          case _ => exist = false
        }
      }
    }
    exist match {
      // 重複の問題がないので、nameの変更。
      case true => {
        player.name(name)
      }
      // 重複の恐れあり。BandSeq内のplayerをチェック
      case _ => {
        val bandseq = BandSeq.findAll(By(BandSeq.id, bandSeq.id.get)).head
        bandseq.players.toList.contains(existPlayers.head) match {
          // 重複あり
          case true => {
            S.error("Duplcate player!")
            S.redirectTo("/member?bandid=" + bandid + "&seq=" + bandseq)
          }
          // 重複がないので、アソシエーションの変更。
          case false =>
            bandseqplayer.player(existPlayers.head.id.get)
        }
      }
    }
    player.save
    bandseqplayer.seq(seq.toLong)
    bandseqplayer.save
    S.notice("updated member " + player.name)
    S.redirectTo("/member?bandid=" + bandid + "&seq=" +bandseq)
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
      val bandSeqPlayers: List[BandSeqPlayers] = BandSeqPlayers.findAll(By(BandSeqPlayers.bandseq, bandseqid), By(BandSeqPlayers.player, playerid))
      bandSeqPlayers match {
        case Nil => ()
        case _ => bandSeqPlayers.head.delete_!
      }
    }

    def duplicateSeqCheck(bandseqid: Long, seq: Long): Boolean = {
      BandSeq.findAll(By(BandSeq.id, bandseqid)).head.bandseqPlayers.filter{ bsp => bsp.seq == seq }.size.equals(0)
    }
    def changeSeqCheck(bandseqPlayersId: Long, seq: Long): Boolean = {
      BandSeqPlayers.findAll(By(BandSeqPlayers.id, bandseqPlayersId)).head.seq.equals(seq)
    }
    def getParent(bandseqid: Long): BandSeq = BandSeq.findAll(By(BandSeq.id, bandseqid)).head
    def getTarget(bandseqid: Long, banseqplayrid: Long): Player = getParent(bandseqid).bandseqPlayers.filter{ bsp => bsp.id == bandseqplayerid}.head.getPlayer
    def getRelation(bandseqplayerid: Long): BandSeqPlayers = BandSeqPlayers.findAll(By(BandSeqPlayers.id, bandseqplayerid)).head
    def getExistTarget(name: String): List[Player] = Player.findAll(By(Player.name, name))
    
    def updateMember(getTarget: (Long, Long) => Target, getParent: Long => Parent, getRelation: Long => Relation, getExistTarget: String => List[Target])(parentId: Long, relationId: Long, name: String, path: String, msg: String, seq: Long): Unit = {
      val target = getTarget(parentId, relationId)
      val relation = getRelation(relationId)
      // 指定されたtargetが既存かどうかチェック。
      //   既存: relationのアソシエーションの変更
      //   未存: targetのnameの更新
      var exist = false
      // 入力されたnameで、targetオブジェクトをインスタンス化
      val existTargets = getExistTarget(name)
      // nameの変更を確認
      target.getName.equals(name) match {
        // 変更が無ければ、重複問題なし
        case true => exist = true
        // 変更の場合、重複の可能性あり
        case _ => {
          // 入力targetオブジェクトの有無確認
          existTargets.size match {
            case 0 => exist = true
            case _ => exist = false
          }
        }
      }
      exist match {
        // 重複の問題がないので、nameの変更。
        case true => {
          target.setName(name)
        }
        // 重複の恐れあり。BandSeq内のplayerをチェック
        case _ => {
          val parent = getParent(parentId)
          parent.getTargets.contains(existTargets.head) match {
            // 重複あり
            case true => {
              S.error(msg)
              S.redirectTo(path)
//              S.error("Duplcate player!")
//              S.redirectTo("/member?bandid=" + grandparentid + "&seq=" + seq)
            }
            // 重複がないので、アソシエーションの変更。
            case false =>
              relation.setPlayer(existTargets.head.getId)
          }
        }
      }
      target.save
      relation.setSeq(seq)
      relation.save
      S.notice(msg)
      S.redirectTo(msg)
//      S.notice("updated member " + target.getName)
//      S.redirectTo("/member?bandid=" + grandparentid + "&seq=" +seq)
    }
}
