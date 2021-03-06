package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Track extends Track with LongKeyedMetaMapper[Track] {
  override def dbTableName = "tracks"
}

class Track extends Target with LongKeyedMapper[Track] with IdPK with ManyToMany with OneToMany[Long, Track] {
  type SuitableObject = Attach
  def this(seq: Long, tracktitle: String) = {
    this()
    this.tracktitle(tracktitle)
  }

  def getSingleton = Track
  override def getName = tracktitle.get
  override def setName(name: String) = this.tracktitle(name)
  override def getId = id.get
  override def getLobs = attaches.toList
  override def getTarget2s = Nil
  override def setBinder(id: Long) = () => ()
  override def getSeq = 0
  override def setLob(attach: SuitableObject) {
    this.attaches += attach
  }
  override def getRelation(relationId: Long) = this.albumTracks.filter{ at => at.id == relationId}.head
  override def validates = this.validate

  object tracktitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "title length must be under 100 characters long ") _  ::
      valMinLen(1, "you have to input the title") _ ::
      super.validations
  }

  object albums extends MappedManyToMany(AlbumTracks, AlbumTracks.track, AlbumTracks.album, Album)

  object attaches extends MappedOneToMany(Attach, Attach.track, OrderBy(Attach.id, Ascending))
  object albumTracks extends MappedOneToMany(AlbumTracks, AlbumTracks.track, OrderBy(AlbumTracks.album, Ascending))

}
