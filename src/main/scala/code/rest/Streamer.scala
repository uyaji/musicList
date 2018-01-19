package code.rest

import net.liftweb.http.StreamingResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.Req
import net.liftweb.common.Full
import java.io._

object Streamer extends RestHelper {
  serve {
    case req@Req(("video" :: id :: Nil), _, _) =>
      val file = new File(id)
      val fis = new FileInputStream(file)
      val size = file.length - 1
      val content_type = "Content-Type" -> "video/mp4"

      val start = 0L
      val end = size

      val headers =
        ("Connection" -> "close") ::
        ("Transfer-Encoding" -> "chunked") ::
        content_type ::
        ("Content-Range" -> ("bytes " + start.toString + "-" + end.toString + "/" + file.length.toString)) ::
        Nil

      () =>  Full(StreamingResponse (
        data = fis,
        onEnd = fis.close,
        size,
        headers,
        cookies = Nil,
        code = 206
      ))
  }
}
