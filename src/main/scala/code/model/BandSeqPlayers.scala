package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandSeqPlayers extends BandSeqPlayers with LongKeyedMetaMapper[BandSeqPlayers] 

class BandSeqPlayers extends Relation with LongKeyedMapper[BandSeqPlayers] with IdPK with OneToMany[Long, BandSeqPlayers] {
  def getSingleton = BandSeqPlayers
//
  override def getId = this.id.get
  override def setSeq(seq: Long) = {
    this.seq(seq)
  }
  override def setTarget(playerid: Long) = {
    this.player(playerid)
  }
  override def getTarget() = {
    this.player.obj.getOrElse(null)
  }
  override def validates = this.validate
//
  object bandseq extends LongMappedMapper(this, BandSeq)
  object player extends LongMappedMapper(this, Player)
  object seq extends MappedLong(this) {
    override def validations =
      minVal _ :: super.validations
    def minVal(in: Long): List[FieldError] =
      if (in > 0 ) Nil
      else List(FieldError(this, <li>Seq must be over 1</li>))
  }
  def getPlayer() = Player.findAll(By(Player.id, player.get)).head
}
