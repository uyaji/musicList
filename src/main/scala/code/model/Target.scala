package code.model

trait Target {
  type SuitableObject <: LargeObject
  type SuitableTarget2 <: Target
  def getName: String
  def setName(name: String):Unit
  def getId: Long
  def getSeq: Int
  def setLob(attach: SuitableObject): Unit
  def setBinder(binderId: Long): Unit
  def getTarget2s: List[SuitableTarget2]
  def getLobs: List[SuitableObject]
  def getRelation(ralationId: Long): Relation
//  def getBinder: SuitableBinder
  def save: Boolean
  def validates: List[net.liftweb.util.FieldError]
}
