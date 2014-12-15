package utils

import play.twirl.api.Html
import utils.BreadCrumbKeeper.UrlReference
import collection.mutable.ListBuffer

object BreadCrumbKeeper {

  case class UrlReference(label: String, uri: String)

  final val noStorageRef = "noStorage"
}

class BreadCrumbKeeper {

  import BreadCrumbKeeper._

  private lazy val breadcrumbs: ListBuffer[UrlReference] = ListBuffer[UrlReference]()
  private lazy val builder = StringBuilder.newBuilder
  private val blacklist =
    List("/administration/substitutions",
      "/administration/students",
      "/administration/users",
      "/administration/labworks",
      "/administration/labworkApplications",
      "/administration/degrees",
      "/administration/courses",
      "/administration/dashboard",
      "/student/dashboard",
      "/administration/blacklist",
      "/administration/rooms",
      "/administration/semesters",
      "/administration/assignments",
      "/administration/substitutions",
      "/administration/live",
      "/superuser",
      "", "/")

  def add(reference: UrlReference): Unit = {
    if (blacklist.contains(reference.uri)) {
      breadcrumbs.clear()
      breadcrumbs += reference
    } else if (breadcrumbs.contains(reference)) {
      val index = breadcrumbs.indexOf(reference)
      breadcrumbs.remove(index + 1, breadcrumbs.size)
    } else {
      breadcrumbs += reference
    }
  }

  def remove(from: Int, amount: Int): Unit = {
    breadcrumbs.remove(from, amount)
  }

  def remove(item: UrlReference): Unit = {
    breadcrumbs - item
  }

  def generate(): Html = {
    builder.clear()

    breadcrumbs.foreach {
      ref ⇒
        builder.append(s"<a href='${ref.uri}'>${ref.label} / </a>")
    }
    Html(builder.toString())
  }
}
