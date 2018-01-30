package code.rest

import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.Req
import net.liftweb.common.Full
import net.liftweb.mapper._
import java.io._
import code.model._

object Streamer extends RestHelper {
  serve {
    case req@Req(("video" :: id :: Nil), _, _) =>
      if(User.currentUser.size == 0) {
        () => Full(RedirectWithState("/user_mgt/login", RedirectState(() => "/user_mgt/login")) )
      }
      else {
      // The under 5 line is for the postgreql.
      try{
        DB.runQuery("set bytea_output='escape'")
      } catch {
        case _: Throwable =>{null}
      }
      val attach: Attach = (Attach.findAll(By(Attach.id, id.toLong))).head
      val bais = new ByteArrayInputStream(attach.trackattach.get)
      val size = attach.trackattach.get.length - 1
      val content_type = "Content-Type" -> attach.mimetype.get

      val start = 0L
      val end = size

      val headers =
        ("Connection" -> "close") ::
      // The under 1 line is a comment out for the heroku.
//        ("Transfer-Encoding" -> "chunked") ::
        content_type ::
        ("Content-Range" -> ("bytes " + start.toString + "-" + end.toString + "/" + attach.trackattach.get.length.toString)) ::
        Nil

      () =>  Full(StreamingResponse (
        data = bais,
        onEnd = bais.close,
        size,
        headers,
        cookies = Nil,
        code = 206
      ))
      }
  }
}
