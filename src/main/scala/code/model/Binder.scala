package code.model

trait Binder {
  type SuitableTarget <: Target
  type SuitableTarget2 <: Target
  def getId: Long
//  def getTargets: List[Target]
  def getTargets: List[SuitableTarget]
  def getTarget2s: List[SuitableTarget2]
  def validate: List[net.liftweb.util.FieldError]
  def save: Boolean
}
