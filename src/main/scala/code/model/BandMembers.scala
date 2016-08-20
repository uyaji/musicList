package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object BandMembers extends BandMembers with MetaMapper[BandMembers]

class BandMembers extends Mapper[BandMembers] {
  def getSingleton = BandMembers
  object band extends LongMappedMapper(this, Band)
  object member extends LongMappedMapper(this, Member)
}
