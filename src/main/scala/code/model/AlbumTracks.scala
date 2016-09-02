package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object AlbumTracks extends AlbumTracks with LongKeyedMetaMapper[AlbumTracks]

class AlbumTracks extends LongKeyedMapper[AlbumTracks] with IdPK {
  def this(album: Long, track: Long, seq: Long) = {
    this()
    this.seq(seq)
    this.album(album)
    this.track(track)
  }
  def getSingleton = AlbumTracks
  object album extends LongMappedMapper(this, Album)
  object track extends LongMappedMapper(this, Track)
  object seq extends MappedLong(this)
  def getTrack(): Track = Track.findAll(By(Track.id, track.get)).head
  def setSeq(seq: Long): Unit = {this.seq(seq)} 
}
