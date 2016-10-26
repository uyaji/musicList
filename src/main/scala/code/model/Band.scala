package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Band extends Band with LongKeyedMetaMapper[Band] {
  override def dbTableName = "bands"
}

class Band extends LongKeyedMapper[Band] with IdPK with OneToMany[Long, Band] {
  def this(bandname: String) = {
    this()
    this.bandname(bandname)
  }

  def getSingleton = Band

  object bandname extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "bandname length must be under 100 characters long ") _ ::
      valMinLen(1, "you have to input bandname") _ ::
      super.validations
  }

  object bandSeqs extends MappedOneToMany(BandSeq, BandSeq.band, OrderBy(BandSeq.seq, Ascending))
  object albums extends MappedOneToMany(Album, Album.band, OrderBy(Album.id, Ascending))
}
