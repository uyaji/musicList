package code.logic

import net.liftweb.common._
import net.liftweb.http._
import S._

object Util {
  def generateSeq(rowCount: Long, getMaxSeq: () => Long, paramSeq: String): String = {
    paramSeq match {
      case "0" => rowCount match {
        case 0 => "1"
        case _ => getMaxSeq().toString
      }
      case _ => paramSeq
    }
  }

  def paramGet(key: String): String = {
    S.param(key) match {
      case Full(value) => value
      case _ => "0"
    }
  }
}
