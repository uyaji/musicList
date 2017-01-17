package code.model

trait Relation{
  def getId: Long
  def setSeq(seq: Long): Unit
  def setTarget(targetid: Long): Unit
  def save: Boolean
  def getTarget(): Target
  def validates: List[net.liftweb.util.FieldError]
}
