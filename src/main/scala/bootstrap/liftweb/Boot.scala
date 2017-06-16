package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import js.jquery.JQueryArtifacts
import sitemap._
import Loc._
import mapper._

import code.model._
import code.snippet._
import net.liftmodules.JQueryModule
import java.io._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Logger {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      sys.props.put("h2.implicitRelativePath", "true")
      val url = (Props.get("db.url"): Option[String]) match {
        // default.propsにdb.urlのキーがあれば、それを採用。
        case Some(url) => url
        // 無ければ、各々から組み立てる。その際に環境変数から値を取得。
        case _ => { 
          val url_prefix = Props.get("db.url_prefix").getOrElse("")
          val host       = System.getenv(Props.get("db.host").getOrElse(""))
          val port       = System.getenv(Props.get("db.port").getOrElse(""))
          val database   = Props.get("db.database").getOrElse("")
          // 2バイト文字を使用出来るように、urlにパラメータを追加。
          url_prefix + host + ":" + port + "/" + database + "?useUnicode=true&characterEncoding=utf8"
        }
      }
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "com.mysql.jdbc.Driver",
/*			     Props.get("db.url") openOr 
			     "jdbc:mysql://localhost:3306/musiclist",*/
                             url,
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Album, Track, Attach, Band, BandSeq, BandSeqPlayers, AlbumTracks, Player, UserAttaches, Message)

    // Download url
    import code.lib._
//    LiftRules.statelessDispatchTable.append{
    LiftRules.statelessDispatch.append{
      case Req( "lob" :: id :: Nil, _, _ ) =>
        () => TrackDownload.download(id.toLong)
      case Req( "suggest" :: key :: key2 :: Nil, _, _ ) =>
        () => Suggest.suggestion(key, key2)
      case Req( "select" :: key :: Nil, _, _ ) =>
        () => Select.rtnOptionList(key)
      case Req( "selectPlayers" :: key1 :: key2 :: Nil, _, _ ) =>
        () => Select.rtnPlayerList(key1, key2)
      case Req( "selectTracks" :: key1 :: key2 :: Nil, _, _ ) =>
        () => Select.rtnTrackList(key1, key2)
      case Req( "selectTwiters" :: id :: Nil, _, _ ) =>
        () => Select.rtnTwiterList(id)

    }
    // where to search snippet
    LiftRules.addToPackages("code")

    // Custom Error page
    LiftRules.exceptionHandler.prepend {
      case (Props.RunModes.Production, Req(path, "", GetRequest), e) => {
        error("Unexpected Exception")
        error(e.fillInStackTrace)
//        println("path = " + new File(".").getAbsoluteFile().getParent())
        RedirectResponse("/static/error")
//        sendErrorPage
      }
    }

    def sendErrorPage = {
      val file = new File("error.html")
      val fis = new FileInputStream(file)
      StreamingResponse(
        fis,
        () => { fis.close },
        file.length,
        ("Content-Type" -> "text/html") :: Nil,
        Nil,
        503
      )
    }
    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index" >> User.AddUserMenusAfter, // the simple way to declare a menu
      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content")), 
      Menu.i("Track") / "track" >> Hidden >> LocGroup("bottom"),
      Menu.i("Band") / "band" >> Hidden >> LocGroup("bottom"),
      Menu.i("Member") / "member" >> Hidden >> LocGroup("bottom"),
      Menu.i("Twit") / "twit" >> Hidden >> LocGroup("bottom")
      )

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery191
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    
    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))    

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    // Upload file size capped at 100Mb
    LiftRules.maxMimeSize = 100 * 1024 * 1024
    LiftRules.maxMimeFileSize = 100 * 1024 * 1024
  }
}
