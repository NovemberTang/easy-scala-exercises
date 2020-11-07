package codechallenge

import scala.util.matching.Regex

object QuestionMarks {

  //Take an input string parameter and determine if exactly 3 question marks exist between every pair of numbers that
  // add up to 10. If so, return true, otherwise return false. Some examples test cases are below:

  def checkAgainstConditions(input: String): Boolean = {
    val matcher:Regex = "[1-9][?]+[1-9]".r
    val result = matcher.findAllIn(input.replaceAll("[a-z]+","")).toList

    val stringsWhereFirstAndLastEqualTen: List[String] = result.filter{
      str => (str.head.toString.toInt + str.last.toString.toInt) == 10
    }

    val contains3QuestionMarks: List[Boolean] = stringsWhereFirstAndLastEqualTen.map(_.drop(1).dropRight(1) == "???")
    checkBoolList(contains3QuestionMarks)
  }

  private def checkBoolList(booleanList: List[Boolean]): Boolean = {
    booleanList.length match {
      case 0 => false
      case 1 => booleanList.head
      case _ => booleanList.forall(_ == true)
    }
  }

}
