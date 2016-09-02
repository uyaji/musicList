package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Album extends Album with LongKeyedMetaMapper[Album] {
  override def dbTableName = "albums"
}

class Album extends LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  def this(albumtitle: String) = {
    this()
    this.albumtitle(albumtitle)
  }
 
  def getSingleton = Album

  object albumtitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "message must be under 100 characters long ") _ ::
      valMinLen(1, "you have to input") _ ::
      super.validations
  }
  object band extends LongMappedMapper(this, Band)

  def getBand(): Band = {
    Band.findAll(By(Band.id, band.get)).head
  } 

  object tracks extends MappedManyToMany(AlbumTracks, AlbumTracks.album, AlbumTracks.track, Track, OrderBy(AlbumTracks.seq, Ascending))
  object albumTracks extends MappedOneToMany(AlbumTracks, AlbumTracks.album, OrderBy(AlbumTracks.seq, Ascending))

}
