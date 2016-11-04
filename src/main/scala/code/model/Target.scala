package code.model

trait Target {
  def getName: String
  def setName(name: String): Unit
  def getId: Long
  def setLob(attach: Attach): Unit
  def save: Boolean
}
