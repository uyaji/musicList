package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Track extends Track with LongKeyedMetaMapper[Track] {
  override def dbTableName = "tracks"
}

class Track extends LongKeyedMapper[Track] with IdPK with OneToMany[Long, Track]{
  def this(album: Long, seq: Long, tracktitle: String) = {
    this()
    this.album(album)
    this.seq(seq)
    this.tracktitle(tracktitle)
  }

  def getSingleton = Track

  object album extends LongMappedMapper(this, Album)
   
  object seq extends MappedLong(this)

  object tracktitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "name length must be under 100 characters long ") _  ::
      valMinLen(1, "you have to input!!") _ ::
      super.validations
  }

  object attaches extends MappedOneToMany(Attach, Attach.track, OrderBy(Attach.id, Ascending))

}
