package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object UserAttaches extends UserAttaches with LongKeyedMetaMapper[UserAttaches] {
  override def dbTableName = "userattaches"
}

class UserAttaches extends LongKeyedMapper[UserAttaches] with IdPK {
  def this(user: Long, attach: Long) = {
    this()
    this.user(user)
    this.attach(attach)
  }
  def getSingleton = UserAttaches
  
  object user extends LongMappedMapper(this, User)
  object attach extends LongMappedMapper(this, Attach)
}
