package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandSeqMembers extends BandSeqMembers with MetaMapper[BandSeqMembers]

class BandSeqMembers extends Mapper[BandSeqMembers] {
  def getSingleton = BandSeqMembers
  object bandseq extends LongMappedMapper(this, BandSeq)
  object member extends LongMappedMapper(this, Member)
}
