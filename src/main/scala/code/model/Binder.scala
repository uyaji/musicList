package code.model

trait Binder {
  type SuitableTarget <: Target
  def getId: Long
//  def getTargets: List[Target]
  def getTargets: List[SuitableTarget]
  def getRelation: scala.collection.mutable.Buffer[Relation]
  def validate: List[net.liftweb.util.FieldError]
}
