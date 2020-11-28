package codechallenge

import scala.util.matching.Regex

object QuestionMarks {

  //Take an input string parameter and determine if exactly 3 question marks exist between every pair of numbers that
  // add up to 10. If so, return true, otherwise return false.

  def checkAgainstConditions(input: String): Boolean = {
    val matcher:Regex = "[1-9][?]{3}[1-9]".r
    val result = matcher.findAllIn(input.replaceAll("[a-z]+","")).toList

    val stringsWhereFirstAndLastEqualTen: List[String] = result.filter{
      str => (str.head.toString.toInt + str.last.toString.toInt) == 10
    }

    stringsWhereFirstAndLastEqualTen.nonEmpty
  }
}
