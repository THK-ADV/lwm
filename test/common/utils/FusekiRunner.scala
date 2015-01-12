package common.utils

import java.io.File
import java.nio.file.Files
import java.util

import org.apache.commons.io.FileUtils
import org.apache.jena.fuseki.FusekiCmd

object FusekiRunner {
  private var maybeServer: Option[Thread] = None

  def runOnce(base: String, port: Int, config: String) = {
    if (maybeServer.isEmpty) {
      FileUtils.cleanDirectory(new File("storeTest"))
      val t = new Thread(new Runnable {
        override def run(): Unit = {
          System.setProperty("log4j.configuration", "log4j.properties")
          new FusekiCmd("--update", s"--desc=$config", s"--port=$port", s"/$base").mainRun()
        }
      })
      t.start()
      maybeServer = Some(t)
    }
  }

}

