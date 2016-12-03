package code.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util._

object Player extends Player with LongKeyedMetaMapper[Player] {
  override def dbTableName = "players"
}

class Player extends Target with LongKeyedMapper[Player] with IdPK with ManyToMany with OneToMany[Long, Player] {
  type SuitableObject = LargeObject
  def this(name: String) = {
    this()
    this.name(name)
  }

  def getSingleton = Player
//
  override def getName = name.get
  override def setName(name: String) = {
    this.name(name)
  }
  override def getId = id.get 
//  override def getLobs = Nil
//  override def setLob(attach: LargeObject) = () => () 
  override def getRelation(relationId: Long) = this.bandSeqPlayers.filter{bsp => bsp.id == relationId}.head
  override def validates = this.validate
//
  object name extends MappedString(this, 80) {
    override def validations =
      valMaxLen(80, "player name length must be under 80 characters long ") _ ::
      valMinLen(1, "you have to input player name") _ ::
      super.validations
  }

  object bandseqs extends MappedManyToMany(BandSeqPlayers, BandSeqPlayers.player, BandSeqPlayers.bandseq, BandSeq) 
  object bandSeqPlayers extends MappedOneToMany(BandSeqPlayers, BandSeqPlayers.player, OrderBy(BandSeqPlayers.bandseq, Ascending))
}
