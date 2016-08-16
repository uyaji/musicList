package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Track extends Track with LongKeyedMetaMapper[Track] {
  override def dbTableName = "tracks"
}

class Track extends LongKeyedMapper[Track] with IdPK {
  def this(albumid: Long, seq: Long, tracktitle: String, filename: String, mimetype: String, lob:Array[Byte]) = {
    this()
    this.albumid(albumid)
    this.seq(seq)
    this.tracktitle(tracktitle)
    this.filename(filename)
    this.mimetype(mimetype)
    this.trackatach(lob)
  }
 
  def this(albumid: Long, seq: Long, tracktitle: String) = {
    this()
    this.albumid(albumid)
    this.seq(seq)
    this.tracktitle(tracktitle)
  }

  def getSingleton = Track

  object albumid extends MappedLong(this)
   
  object seq extends MappedLong(this)

  object tracktitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "name length must be under 100 characters long ") _  ::
      valMinLen(1, "you have to input!!") _ ::
      super.validations
  }

  object filename extends MappedString(this, 100)
  object mimetype extends MappedString(this, 40)

  object trackatach extends MappedBinary(this)
}
