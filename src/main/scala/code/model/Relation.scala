package code.model

trait Relation{
  def setSeq(seq: Long): Unit
  def setPlayer(targetid: Long): Unit
  def save: Boolean
}
