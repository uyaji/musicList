package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date

object BandSeq extends BandSeq with LongKeyedMetaMapper[BandSeq] {
  override def dbTableName = "bandseqs"
}

class BandSeq extends Binder with LongKeyedMapper[BandSeq] with IdPK with ManyToMany with OneToMany[Long, BandSeq] {
  def this(bandSeqStartAt: Date, bandSeqEndAt: Date, seq: Int) = {
    this()
    this.bandSeqStartAt(bandSeqStartAt)
    this.bandSeqEndAt(bandSeqEndAt)
    this.seq(seq)
  }

  def this(bandSeqStartAt: Date, bandSeqEndAt: Date) = {
    this()
    this.bandSeqStartAt(bandSeqStartAt)
    this.bandSeqEndAt(bandSeqEndAt)
  }

  def getSingleton = BandSeq
  override def getTargets = players.toList
  override def getRelation = bandseqPlayers.asInstanceOf[scala.collection.mutable.Buffer[Relation]]

  object bandSeqStartAt extends MappedDateTime(this)
  object bandSeqEndAt extends MappedDateTime(this)
  object seq extends MappedInt(this) {
    override def defaultValue = 1
  }

  def getBand(): Band = Band.findAll(By(Band.id, band)).head
  object players extends MappedManyToMany(BandSeqPlayers, BandSeqPlayers.bandseq, BandSeqPlayers.player, Player, OrderBy(BandSeqPlayers.seq, Ascending))
  object band extends LongMappedMapper(this, Band)
  object bandseqPlayers extends MappedOneToMany(BandSeqPlayers, BandSeqPlayers.bandseq, OrderBy(BandSeqPlayers.seq, Ascending))
  object albums extends MappedOneToMany(Album, Album.bandseq, OrderBy(Album.id, Ascending))
}
