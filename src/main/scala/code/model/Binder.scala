package code.model

trait Binder {
  def getTargets: List[Target]
  def getRelation: scala.collection.mutable.Buffer[Relation]
  def validate: List[net.liftweb.util.FieldError]
}
