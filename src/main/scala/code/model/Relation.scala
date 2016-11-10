package code.model

trait Relation{
  def setSeq(seq: Long): Unit
  def setTarget(targetid: Long): Unit
  def save: Boolean
  def getTarget(): Target
}
