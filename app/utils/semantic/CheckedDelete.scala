package utils.semantic

import utils.{ QueryHost, UpdateHost }
import scala.concurrent.{ Promise, blocking }

trait CheckedDelete {
  import utils.Implicits._

  def delete(resource: Resource)(implicit updateHost: UpdateHost, queryHost: QueryHost) = {
    val p = Promise[Resource]()
    blocking {
      if (check(resource)) {
        SPARQLBuilder.removeIndividual(resource).execUpdate()
        p.success(resource)
      }
    }
    p.future
  }

  def check(resource: Resource)(implicit queryHost: QueryHost): Boolean
}