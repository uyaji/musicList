package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandSeqPlayers extends BandSeqPlayers with LongKeyedMetaMapper[BandSeqPlayers]

class BandSeqPlayers extends LongKeyedMapper[BandSeqPlayers] with IdPK{
  def getSingleton = BandSeqPlayers
  object bandseq extends LongMappedMapper(this, BandSeq)
  object player extends LongMappedMapper(this, Player)
  object seq extends MappedLong(this)
}
