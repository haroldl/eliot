package controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.Date

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
          val searchReq1 = searchUsers(q, 1)
          val searchReq2 = searchUsers(q, 2)
          for {
            users1 <- searchReq1
            users2 <- searchReq2
          } yield {
            val users = (users1 ++ users2) map TwitterUser
            Ok(views.html.search(q, users))
          }
        }
      }
    }
  }

  case class TwitterUser(val json: JsObject) {
    // Helpers for extracting Twitter fields from user objects
    def handle = (json \ "screen_name").as[String]
    def userName = (json \ "name").as[String]
    def description = (json \ "description").as[String]
    def location = (json \ "location").as[String]
    def tweets = (json \ "statuses_count").as[Int]
    def followed = (json \ "following").as[Boolean]
    def followers = (json \ "followers_count").as[Int]
    def following = (json \ "friends_count").as[Int]
    def followersStr = "%,d".format(followers)
    def followingStr = "%,d".format(following)
    def ratioFollowingToFollowers = following.toDouble / followers.toDouble
  }

  // The rate limit ceiling for that given request.
  val RESPONSE_HEADER_RATE_LIMIT = "X-Rate-Limit-Limit"

  // The number of requests left for the 15 minute window.
  val RESPONSE_HEADER_RATE_LIMIT_REMAINING = "X-Rate-Limit-Remaining"

  // The remaining window before the rate limit resets in UTC epoch seconds.
  val RESPONSE_HEADER_RATE_LIMIT_RESET = "X-Rate-Limit-Reset"

  def following(implicit request: RequestHeader, token: OAuthCalculator): Future[Seq[JsObject]] = {
    twitterGet("/friends/list.json").map { response =>
      throwOnRateLimiting(response)
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
      throwOnRateLimiting(response)
      response.json.as[JsArray].value.map(_.as[JsObject])
    }
  }

  def throwOnRateLimiting(response: WSResponse) {
    if (response.status == 429) {
      throw new Exception("Twitter rate limiting!")
    }
    val limit: Option[String] = response.header(RESPONSE_HEADER_RATE_LIMIT)
    val remaining: Option[String] = response.header(RESPONSE_HEADER_RATE_LIMIT_REMAINING)
    val reset: Option[Date] = response.header(RESPONSE_HEADER_RATE_LIMIT_RESET).map {
      str => new Date(str.toLong * 1000)
    }
    Logger.info(s"Twitter rate limiting stats: limit: $limit remaining: $remaining reset: $reset")
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