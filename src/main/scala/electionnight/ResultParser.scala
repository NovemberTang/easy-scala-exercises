package electionnight

import scala.util.{Failure, Success, Try}

object ResultParser {

  private def voteListToMap(list: List[String]) = {

    Map(list(1) -> list(0).toInt)

  }

  private def lineToConstituencyScore(string: String): ConstituencyScore = {
    require(string.contains(", "))
    val lineArray = string.split(", ")
    val (constituency, votes) = (lineArray.head, lineArray.tail.sliding(2, 2).map(_.toList))

    val voteMap: Map[String,Int] = votes.flatMap(voteListToMap).toMap.withDefaultValue(0)

    new ConstituencyScore(constituency = constituency,voteMap)

  }

  def readLineWithErrorHandling(string: String): Option[String] = {
    val x = Try(lineToConstituencyScore(string))
    x match {
      case Success(constituencyScore) => Some(constituencyScore.toString + "\n")
      case Failure(_) => {
        val constituency = string.split(", ").headOption
        if (constituency.isDefined) {
          println(s"Failed to get data for <${constituency.get}>")
          None
        }
        else {
          println("Failed to get data for an unknown constituency")
          None
        }

      }
    }
  }

  def parseAllResults(results: Iterator[String]): Iterator[String] = {
    results.flatMap(readLineWithErrorHandling)
  }

}
