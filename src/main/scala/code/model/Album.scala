package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Album extends Album with LongKeyedMetaMapper[Album] {
  override def dbTableName = "albums"
}

class Album extends LongKeyedMapper[Album] with IdPK {
  def this(albumtitle: String, artistname: String) = {
    this()
    this.albumtitle(albumtitle)
    this.artistname(artistname)
  }
 
  def getSingleton = Album

  object albumtitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "message must be under 100 characters long ") _ ::
      valMinLen(1, "you have to input") _ ::
      super.validations
  }

  object artistname extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "name length must be under 100 characters long ") _  ::
      valMinLen(1, "you have to input") _ ::
      super.validations
  }
}
