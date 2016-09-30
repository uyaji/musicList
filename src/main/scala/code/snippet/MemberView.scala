package code.snippet
import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import code.model.Band

class MemberView {
  val bandid = getParam("bandid")
  val seq = getParam("seq")
  val band = Band.findAll(By(Band.id, bandid.toLong)).head

  def bandNameSeq(html: NodeSeq): NodeSeq = {
    bind("band", html, "nameseq" -> (band.bandname + " : " + seq ))
  }

  private
    def getParam(key: String): String = {
      S.param(key) match {
        case Full(value) => value
        case _ => "none"
      }
    }
}
