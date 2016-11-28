package code.model

trait Target {
  type SuitableObject <: LargeObject
  def getName: String
  def setName(name: String): Unit
  def getId: Long
  def setLob(attach: SuitableObject): Unit
  def getLobs: List[SuitableObject]
  def getRelation(ralationId: Long): Relation
  def save: Boolean
  def validates: List[net.liftweb.util.FieldError]
}
