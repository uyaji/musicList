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
    val bais = new ByteArrayInputStream(track.trackatach.get)
    val attachment = "attachment; filename=\'" + MimeUtility.encodeWord(track.filename.get.replace(" ", "_"), "ISO-2022-JP", "B") + "\'"
    val content = track.mimetype.get + "; charset=UTF-8"
    val headers = ("Content-Type" -> content) :: ("Content-length" -> track.trackatach.get.length.toString) ::("Content-disposition" -> attachment) :: Nil
    Full(
      StreamingResponse(bais, () => {bais.close}, track.trackatach.get.length, headers, Nil, 200)
    )
  }
}
