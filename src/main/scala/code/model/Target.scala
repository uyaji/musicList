package code.model

trait Target {
  def getName: String
  def setName(name: String): Unit
  def getId: Long
  def setLob(attach: Attach): Unit
  def getRelation(ralationId: Long): Relation
  def save: Boolean
  def validates: List[net.liftweb.util.FieldError]
}
