package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

object Message extends Message with LongKeyedMetaMapper[Message] {
  override def fieldOrder = List(id)
}

class Message extends LongKeyedMapper[Message] with IdPK {
  def getSingleton = Message

  object status extends MappedTextarea(this, 140) {
    override def textareaCols = 40
    override def textareaRows = 4

    override def validations = 
      valMaxLen( 140, "The message must be under 140 characters long!") _ ::
      valMinLen(1, "you have to input the message!") _ ::
      super.validations
  }
  
  object from extends MappedLongForeignKey(this, User)

  object to extends MappedLongForeignKey(this, User)

  def fromUser = User.findAll(By(User.id, from)).head
  def toUser = User.findAll(By(User.id, to)).head
}
