package controllers

import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import play.api.Configuration


object UserInfoManagement{
  case class UserNameRequest(id: String)
  case class UserNotesRequest(id: String)
  case class UserNoteAddition(id: String, note: String)

  def props(config: Configuration) = Props(new UserInfoManagement(config))
}

class UserInfoManagement(config: Configuration) extends Actor{
  import UserInfoManagement._
  import utils.LDAPAuthentication._
  import scala.concurrent.ExecutionContext.Implicits.global

  val DN = config.getString("lwm.bindDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get


  override def receive: Receive = {
    case UserNameRequest(id) =>
      val requester = sender()
      for(name <- nameInfo(id, bindHost, bindPort, DN)){
        name match{
          case Left(error) => requester ! id
          case Right(n) => requester ! n
        }
      }
    case UserNotesRequest(id) =>
      // TODO
  }
}
