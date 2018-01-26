package code.lib

import java.io._
import javax.mail.internet._
import net.liftweb.common._

import code.model.Attach
import net.liftweb.mapper._
import net.liftweb.http._

object TrackDownload {
  def download(id: Long): Box[LiftResponse] = {
    // The under 5 line is for the postgreql.
    try{
      DB.runQuery("set bytea_output='escape'")
    } catch {
      case _: Throwable =>{null}
    }
    val attach: Attach = (Attach.findAll(By(Attach.id, id))).head
    val bais = new ByteArrayInputStream(attach.trackattach.get)
    val attachment = "attachment; filename=\'" + MimeUtility.encodeWord(attach.filename.get.replace(" ", "_"), "ISO-2022-JP", "B") + "\'"
    val content = attach.mimetype.get + "; charset=UTF-8"
    val headers = ("Content-Type" -> content) :: ("Content-length" -> attach.trackattach.get.length.toString) ::("Content-disposition" -> attachment) :: Nil
    Full(
      StreamingResponse(bais, () => {bais.close}, attach.trackattach.get.length, headers, Nil, 200)
    )
  }
}
