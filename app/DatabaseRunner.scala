import org.apache.jena.fuseki.FusekiCmd

object DatabaseRunner extends App{
  new Thread(new Runnable {
    override def run(): Unit = {
      System.setProperty("log4j.configuration", "log4j.properties")
      new FusekiCmd("--update", "--desc=assemblerA.ttl", "--port=3030", "/lwm").mainRun()
    }
  }).start()
}
