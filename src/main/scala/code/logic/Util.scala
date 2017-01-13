package code.logic

import net.liftweb.common._
import net.liftweb.http._
import S._
import java.text.SimpleDateFormat
import java.util.Date

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

  def stringToDate(strDate: String): Date = {
    try {
      val sdf = new SimpleDateFormat("yyyy");
      sdf.parse(strDate.substring(0,4))
    } catch {
       case e: Exception => new Date(0)
    }
  }

  def dateToString(date: Date): String = {
    val sdf = new SimpleDateFormat("yyyy");
    sdf.format(date)
  }

  def isAllDigits(s: String) = s match {
    case "" => false
    case p: String => p forall Character.isDigit
  }
}
