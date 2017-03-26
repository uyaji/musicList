package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import scala.math.Ordered

object Album extends Album with LongKeyedMetaMapper[Album] with CRUDify[Long, Album] {
  override def dbTableName = "albums"
}

class Album extends Binder with Target with LongKeyedMapper[Album] with IdPK with ManyToMany with OneToMany[Long, Album] with Ordered[Album] {
  import scala.math.Ordered._
  type SuitableTarget = Track
  def this(albumtitle: String) = {
    this()
    this.albumtitle(albumtitle)
  }
 
  def compare(that: Album) = {
    (getBandSeq.getBand.bandname.get + albumtitle.get).compareToIgnoreCase(that.getBandSeq.getBand.bandname.get + that.albumtitle.get)
  }
  def getSingleton = Album
  override def getId = id.get
  override def getSeq = 0
  override def getTargets = tracks.toList
  override def getTarget2s = Nil
  override def getName = albumtitle.get
  override def setName(name: String) = this.albumtitle(name)
  override def getRelation(relationId: Long) = null
  override def setLob(attach: SuitableObject) = () => ()
  override def getLobs: List[SuitableObject] = Nil
  override def setBinder(id: Long) = this.bandseq(id)
  override def validates = this.validate

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
