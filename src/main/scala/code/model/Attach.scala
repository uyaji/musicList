package code.model

import net.liftweb.mapper._

object Attach extends Attach with LongKeyedMetaMapper[Attach] {
  override def dbTableName = "attaches"
}

class Attach extends LongKeyedMapper[Attach] with IdPK {
  def getSingleton = Attach

  def this(filename: String, mimetype: String, trackattach: Array[Byte]) = {
    this()
    this.filename(filename)
    this.mimetype(mimetype)
    this.trackattach(trackattach)
  }

  object filename extends MappedString(this, 100)
  object mimetype extends MappedString(this, 40)
  object trackattach extends MappedBinary(this)
  object track extends LongMappedMapper(this, Track)
}
