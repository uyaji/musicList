package code.logic

import net.liftweb.common._
import net.liftweb.http._
import S._
import java.text.SimpleDateFormat
import java.util.Date

import code.model._

object MockUtil extends TraitUtil{
  def generateSeq(rowCount: Long, getMaxSeq: () => Long, paramSeq: String): String = {
    "1"
  }

  def paramGet(key: String): String = {
    "1"
  }

  def stringToDate(strDate: String): Date = {
    new Date(0)
  }

  def dateToString(date: Date): String = {
    "19700101"
  }

  def isAllDigits(s: String): Boolean = {
    true
  }

  def isSuperUser(): Boolean  = {
    false
  }
}
