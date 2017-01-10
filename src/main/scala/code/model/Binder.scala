package code.model

trait Binder {
  type SuitableTarget <: Target
  def getId: Long
//  def getTargets: List[Target]
  def getTargets: List[SuitableTarget]
  def validate: List[net.liftweb.util.FieldError]
}
