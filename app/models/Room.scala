package models

import utils.semantic.Resource


object Room {

  val rooms = List(Room(Resource("2103")), Room(Resource("2102")))
}

case class Room(number: Resource)
