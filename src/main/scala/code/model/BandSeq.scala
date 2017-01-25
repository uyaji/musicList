package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date

object BandSeq extends BandSeq with LongKeyedMetaMapper[BandSeq] {
  override def dbTableName = "bandseqs"
}

class BandSeq extends Binder with Target with LongKeyedMapper[BandSeq] with IdPK with ManyToMany with OneToMany[Long, BandSeq] {
  type SuitableTarget = Player
  type SuitableTarget2 = Album
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
  override def getId = id.get
  override def getSeq = seq.get
  override def getTargets = players.toList
  override def getTarget2s = albums.toList

  override def getName = ""
  override def setName(name: String) = () => ()
  override def setLob(attach: SuitableObject) = () => ()
  override def getLobs: List[SuitableObject] = Nil
  override def getRelation(id: Long) = null
  override def setBinder(id: Long) = () => ()
  override def validates = this.validate

  object bandSeqStartAt extends MappedDateTime(this)
  object bandSeqEndAt extends MappedDateTime(this)
  object seq extends MappedInt(this) {
    override def defaultValue = 1
    override def validations =
      minVal _ :: super.validations
    def minVal(in: Int): List[FieldError] =
      if (in > 0 ) Nil
      else List(FieldError(this, <li>Seq must be over 1</li>))
  }

  def getBand(): Band = Band.findAll(By(Band.id, band.get)).head
  object players extends MappedManyToMany(BandSeqPlayers, BandSeqPlayers.bandseq, BandSeqPlayers.player, Player, OrderBy(BandSeqPlayers.seq, Ascending))
  object band extends LongMappedMapper(this, Band)
  object bandseqPlayers extends MappedOneToMany(BandSeqPlayers, BandSeqPlayers.bandseq, OrderBy(BandSeqPlayers.seq, Ascending))
  object albums extends MappedOneToMany(Album, Album.bandseq, OrderBy(Album.id, Ascending))
}
