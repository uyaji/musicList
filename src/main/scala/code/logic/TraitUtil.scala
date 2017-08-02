package code.logic
import java.util.Date

trait TraitUtil{
  def generateSeq(rowCount: Long, getMaxSeq: () => Long, paramSeq: String): String
  def paramGet(key: String): String

  def stringToDate(strDate: String): Date

  def dateToString(date: Date): String

  def isAllDigits(s: String): Boolean

  def isSuperUser(): Boolean
}
