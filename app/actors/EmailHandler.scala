package actors

import javax.mail.search.{ RecipientStringTerm, RecipientTerm, SearchTerm }

import actors.EmailHandler.EmailResponse
import akka.actor.{ Props, Actor }
import javax.mail._
import javax.mail.internet.{ MimeMultipart, InternetAddress }
import javax.mail.Message.RecipientType

import com.sun.mail.imap.IMAPFolder
import play.api.Configuration

sealed trait EmailContent {
  val content: String
}
case class PlainTextContent(content: String) extends EmailContent
case class HTMLContent(content: String) extends EmailContent

object EmailHandler {

  case class MessageRequest(count: Int)

  case class EmailResponse(messages: List[Message])

  def props(config: Configuration) = Props(new EmailHandler(config: Configuration))
}

class EmailHandler(config: Configuration) extends Actor {
  val user = config.getString("lwm.email.user").get
  val password: String = config.getString("lwm.email.password").get
  val host: String = config.getString("lwm.email.host").get
  val protocol: String = config.getString("lwm.email.protocol").get
  val folderName: String = config.getString("lwm.email.folder").get

  val props = System.getProperties
  props.setProperty("mail.store.protocol", protocol)

  val session = Session.getDefaultInstance(props, null)
  session.setDebug(false)
  val store = session.getStore(protocol)
  store.connect(host, user, password)
  val folder = store.getFolder(folderName).asInstanceOf[IMAPFolder]
  folder.open(Folder.READ_ONLY)

  def receive: Actor.Receive = {
    case EmailHandler.MessageRequest(count) ⇒
      folder.close(false)
      folder.open(Folder.READ_ONLY)
      val latest = folder.search(new RecipientStringTerm(RecipientType.TO, "ap-praktikum@gm.fh-koeln.de")).takeRight(count)
      sender() ! EmailResponse(latest.toList)

  }

  def extractText(part: Part): List[EmailContent] = {
    if (part.isMimeType("text/plain")) {
      return PlainTextContent(part.getContent.asInstanceOf[String]) :: Nil
    }
    if (part.isMimeType("text/html")) {
      return HTMLContent(part.getContent.asInstanceOf[String]) :: Nil
    }
    if (part.isMimeType("multipart/*")) {
      part.getContent match {
        case mime: MimeMultipart ⇒
          var texts = List[EmailContent]()
          for (i ← 0 until mime.getCount) {
            val bp = mime.getBodyPart(i)
            texts = texts ::: extractText(bp)
          }
          return texts
        case _ ⇒
      }
    }
    Nil
  }

}
