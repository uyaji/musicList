package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Track extends Track with LongKeyedMetaMapper[Track] {
  override def dbTableName = "tracks"
}

class Track extends LongKeyedMapper[Track] with IdPK with ManyToMany with OneToMany[Long, Track] {
  def this(seq: Long, tracktitle: String) = {
    this()
    this.tracktitle(tracktitle)
  }

  def getSingleton = Track

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
