package code.lib

import java.io._
import javax.mail.internet._
import net.liftweb.common._

import code.model.Track
import net.liftweb.mapper._
import net.liftweb.http._

object TrackDownload {
  def download(id: Long): Box[LiftResponse] = {
    val track: Track = (Track.findAll(By(Track.id, id))).head
    val bais = new ByteArrayInputStream(track.attaches(0).trackattach.get)
    val attachment = "attachment; filename=\'" + MimeUtility.encodeWord(track.attaches(0).filename.get.replace(" ", "_"), "ISO-2022-JP", "B") + "\'"
    val content = track.attaches(0).mimetype.get + "; charset=UTF-8"
    val headers = ("Content-Type" -> content) :: ("Content-length" -> track.attaches(0).trackattach.get.length.toString) ::("Content-disposition" -> attachment) :: Nil
    Full(
      StreamingResponse(bais, () => {bais.close}, track.attaches(0).trackattach.get.length, headers, Nil, 200)
    )
  }
}
