package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object AlbumTracks extends AlbumTracks with LongKeyedMetaMapper[AlbumTracks]

class AlbumTracks extends Relation with LongKeyedMapper[AlbumTracks] with IdPK {
  def this(album: Long, track: Long, seq: Long) = {
    this()
    this.seq(seq)
    this.album(album)
    this.track(track)
  }

  def getSingleton = AlbumTracks
  override def getId = this.id.get
  override def setSeq(seq: Long) = this.seq(seq)
  override def setTarget(trackid: Long) = this.track(trackid)
  override def getTarget() = this.track.asInstanceOf[Target]

  object album extends LongMappedMapper(this, Album)
  object track extends LongMappedMapper(this, Track)
  object seq extends MappedLong(this) {
    override def validations = 
      minVal _ :: super.validations
    def minVal(in: Long): List[FieldError] = 
      if (in > 0 ) Nil 
      else List(FieldError(this, <div>Seq must be over 1</div>))
  }
  def getTrack(): Track = Track.findAll(By(Track.id, track.get)).head
}
