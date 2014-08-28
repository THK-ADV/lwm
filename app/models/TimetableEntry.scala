package models

import utils.semantic.Individual

/**
 * Created by root on 8/28/14.
 */


object TimetableEntry {

}

case class TimetableEntry(name: String, supervisors: List[String], room: String, day: String, time: String)

