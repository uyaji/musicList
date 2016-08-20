package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Member extends Member with LongKeyedMetaMapper[Member] {
  override def dbTableName = "members"
}

class Member extends LongKeyedMapper[Member] with IdPK with ManyToMany {
  def this(membername: String) = {
    this()
    this.membername(membername)
  }

  def getSingleton = Member

  object membername extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "name length must be under 100 characters long ") _  ::
      valMinLen(1, "you have to input!!") _ ::
      super.validations
  }

  object bands extends MappedManyToMany(BandMembers, BandMembers.member, BandMembers.band, Band)
}
