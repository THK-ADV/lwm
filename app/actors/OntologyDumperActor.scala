package actors

import java.io.{ File, FileOutputStream, ByteArrayInputStream, ByteArrayOutputStream }
import java.nio.file.Files

import akka.actor.{ Props, Actor }
import akka.actor.Actor.Receive
import com.hp.hpl.jena.query.DatasetAccessorFactory
import org.joda.time.LocalDateTime

import scala.concurrent.duration.FiniteDuration

object OntologyDumperActor {
  case object DumpRequest

  def props(dataUrl: String, interval: FiniteDuration) = Props(new OntologyDumperActor(dataUrl, interval))
}

class OntologyDumperActor(dataUrl: String, interval: FiniteDuration) extends Actor {
  import scala.concurrent.duration._
  import OntologyDumperActor._
  import context.dispatcher

  val dataAccess = DatasetAccessorFactory.createHTTP(dataUrl)
  val folder = new File("dump")
  if (!folder.exists()) {
    Files.createDirectory(folder.toPath)
  }

  context.system.scheduler.schedule(3.seconds, interval, self, DumpRequest)
  override def receive: Receive = {
    case DumpRequest ⇒
      val unionModel = dataAccess.getModel
      val os = new ByteArrayOutputStream()
      unionModel.write(os, "TTL")

      val f = new File(folder, s"lwm_dump_${LocalDateTime.now().toString("yyyy-MM-dd-(HH-mm-ss)")}.ttl")
      if (!f.exists()) {
        Files.createFile(f.toPath)
      }

      new FileOutputStream(f).write(os.toByteArray)
  }
}
