package code.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util._

object Player extends Player with LongKeyedMetaMapper[Player] {
  override def dbTableName = "players"
}

class Player extends LongKeyedMapper[Player] with IdPK with ManyToMany {
  def this(name: String) = {
    this()
    this.name(name)
  }

  def getSingleton = Player

  object name extends MappedString(this, 80) {
    override def validations =
      valMaxLen(80, "name length must be under 80 characters long ") _ ::
      valMinLen(1, "you have to input!!") _ ::
      super.validations
  }

  object bandseqs extends MappedManyToMany(BandSeqPlayers, BandSeqPlayers.player, BandSeqPlayers.bandseq, BandSeq) 
}
