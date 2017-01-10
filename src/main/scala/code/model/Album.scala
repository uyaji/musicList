package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Album extends Album with LongKeyedMetaMapper[Album] {
  override def dbTableName = "albums"
}

class Album extends Binder with LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] {
  type SuitableTarget = Track
  def this(albumtitle: String) = {
    this()
    this.albumtitle(albumtitle)
  }
 
  def getSingleton = Album
  override def getId = id.get
  override def getTargets = tracks.toList

  object albumtitle extends MappedString(this, 100) {
    override def validations =
      valMaxLen(100, "title must be under 100 characters long ") _ ::
      valMinLen(1, "you have to input title") _ ::
      super.validations
  }
  object bandseq extends LongMappedMapper(this, BandSeq)

  def getBandSeq(): BandSeq = {
    BandSeq.findAll(By(BandSeq.id, bandseq.get)).head
  } 

  object tracks extends MappedManyToMany(AlbumTracks, AlbumTracks.album, AlbumTracks.track, Track, OrderBy(AlbumTracks.seq, Ascending))
  object albumTracks extends MappedOneToMany(AlbumTracks, AlbumTracks.album, OrderBy(AlbumTracks.seq, Ascending))

}
