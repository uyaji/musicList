package code.snippet

import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import code.model.Band

class BandView {
  val bandid = getBandId()
  val band = Band.findAll(By(Band.id, bandid.toLong)).head

  def bandName(html: NodeSeq): NodeSeq = {
    bind("band", html, "name" -> band.bandname)
  }

  private def getBandId(): String = {
    S.param("bandid") match {
      case Full(id) => id
      case _ => "0"
    }
  }
}
