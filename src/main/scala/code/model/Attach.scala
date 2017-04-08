package code.model

import net.liftweb.mapper._

object Attach extends Attach with LongKeyedMetaMapper[Attach] {
  override def dbTableName = "attaches"
}

class Attach extends LargeObject with LongKeyedMapper[Attach] with IdPK with ManyToMany {
//class Attach extends LongKeyedMapper[Attach] with IdPK {
  def getSingleton = Attach

//  override def getFileName = this.filename.get
  def this(filename: String, mimetype: String, trackattach: Array[Byte], valid: Boolean, uploader: Long) = {
    this()
    this.filename(filename)
    this.mimetype(mimetype)
    this.trackattach(trackattach)
    this.valid(valid)
    this.uploader(uploader)
  }

  object filename extends MappedString(this, 100)
  object mimetype extends MappedString(this, 40)
  object trackattach extends MappedBinary(this)
  object valid extends MappedBoolean(this)
  object track extends LongMappedMapper(this, Track)
  def getTrack(): Track = {
    Track.findAll(By(Track.id, track.get)).head
  }
  override def getFileName(): String = filename.get
  object users extends MappedManyToMany(UserAttaches, UserAttaches.attach, UserAttaches.user, User, OrderBy(UserAttaches.user, Ascending))
  object uploader extends LongMappedMapper(this, User)

  def getUploader(): User = {
    User.findAll(By(User.id, uploader.get)).head
  }
}
