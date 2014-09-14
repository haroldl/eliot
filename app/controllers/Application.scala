package controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api._
import play.api.libs.json.{JsObject, JsArray, JsValue}
import play.api.libs.oauth.{RequestToken, OAuthCalculator}
import play.api.libs.ws.{WSResponse, WS}
import play.api.mvc._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import scala.Some
import scala.concurrent.Future

object Application extends Controller {

  def index = Action.async { implicit request =>
    withTwitterToken { implicit token =>
      for { users <- following } yield {
        val message = users.map(_ \ "screen_name").toString
        Ok(views.html.index("Your new application is ready; " + message))
      }
    }
  }

  def search = Action.async { implicit request =>
    withTwitterToken { implicit token =>
      val query = request.body.asFormUrlEncoded.flatMap(_.get("q")).map(_(0))
      query match {
        case None => Future.successful(Redirect(routes.Application.index))
        case Some(q) => {
          val followingReq = following
          val searchReq1 = searchUsers(q, 1)
          val searchReq2 = searchUsers(q, 2)
          for {
            users1 <- searchReq1
            users2 <- searchReq2
            followedUsers <- followingReq
          } yield {
            val users = users1 ++ users2
            val followedNames = followedUsers.map(_ \ "screen_name").toSet
            val userNames = users.toSeq map TwitterUser

            Ok(views.html.search(q, userNames))
          }
        }
      }
    }
  }

  case class TwitterUser(val json: JsObject) {
    // Helpers for extracting Twitter fields from user objects
    def handle = (json \ "screen_name").as[String]
    def userName = (json \ "name").as[String]
    def location = (json \ "location").as[String]
    def followers = (json \ "followers").as[Int]
  }

  def following(implicit request: RequestHeader, token: OAuthCalculator): Future[Seq[JsObject]] = {
    twitterGet("/friends/list.json").map { response =>
      (response.json \ "users").as[JsArray].value.map { _.as[JsObject] }
    }
  }

  def searchUsers(query: String, page: Int = 1, count: Int = 20)
                 (implicit request: RequestHeader, token: OAuthCalculator): Future[Seq[JsObject]] = {
    if (page <= 0)
      throw new IllegalArgumentException("Pagination via the Twitter API starts with page=1")
    if (count < 1 || count > 20)
      throw new IllegalArgumentException("Pagination via the Twitter API requires the count to be between 1 and 20, inclusive.")
    val encodedQuery = URLEncoder.encode(query, "UTF-8")
    twitterGet(s"/users/search.json?q=${encodedQuery}&page=${page}&count=${count}").map { response =>
      response.json.as[JsArray].value.map(_.as[JsObject])
    }
  }

  def requestToken()(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield RequestToken(token, secret)
  }

  def twitterGet(path: String)(implicit request: RequestHeader, oauth: OAuthCalculator): Future[WSResponse] = {
    requestToken() match {
      case Some(token) => {
        val url = "https://api.twitter.com/1.1" + path
        WS.url(url).sign(oauth).get
      }
      case None => Future.failed(new Exception(""))
    }
  }

  def withTwitterToken(code: OAuthCalculator => Future[Result])(implicit request: RequestHeader): Future[Result] = {
    requestToken() match {
      case Some(token) => {
        val oauth = OAuthCalculator(Twitter.KEY, token)
        code(oauth)
      }
      case None => Future.successful(Redirect(routes.Twitter.authenticate))
    }
  }
}