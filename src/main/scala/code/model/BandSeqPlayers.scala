package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandSeqPlayers extends BandSeqPlayers with MetaMapper[BandSeqPlayers]

class BandSeqPlayers extends Mapper[BandSeqPlayers] {
  def getSingleton = BandSeqPlayers
  object bandseq extends LongMappedMapper(this, BandSeq)
  object player extends LongMappedMapper(this, Player)
  object seq extends MappedLong(this)
}
