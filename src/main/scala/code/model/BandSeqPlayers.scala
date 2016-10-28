package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandSeqPlayers extends BandSeqPlayers with LongKeyedMetaMapper[BandSeqPlayers] with Relation {
  override def setSeq(seq: Long) = {
    this.seq(seq)
  }
  override def setPlayer(playerid: Long) = {
    this.player(playerid)
  }
  override def save = this.save
}

class BandSeqPlayers extends LongKeyedMapper[BandSeqPlayers] with IdPK{
  def getSingleton = BandSeqPlayers
  object bandseq extends LongMappedMapper(this, BandSeq)
  object player extends LongMappedMapper(this, Player)
  object seq extends MappedLong(this) {
    override def validations =
      minVal _ :: super.validations
    def minVal(in: Long): List[FieldError] =
      if (in > 0 ) Nil
      else List(FieldError(this, <b>Seq must be over 1</b>))
  }
  def getPlayer(): Player = Player.findAll(By(Player.id, player.get)).head
}
