package codechallenge

import scala.math.abs

object WeightBalancing {

  def balanceTheWeight(scalesString: String, weightsString: String): String = {
    val scales: List[Int] = convertStringListToIntList(scalesString)
    require(scales.length == 2, "Scales must have a length of two.")
    val weights: List[Int] = convertStringListToIntList(weightsString)
    val weightDiff = abs(scales.head - scales.last)

    val try1 = findOneOnOneSide(weights, weightDiff)
    try1 match {
      case Some(x) => convertIntListToStringList(List(x))
      case None =>
        val try2 = findOneOnEachSide(weights, weightDiff)
        try2 match {
          case Some(list) => convertIntListToStringList(list)
          case None =>
            val try3 = findTwoOnOneSide(weights, weightDiff)
            try3 match {
              case Some(list) => convertIntListToStringList(list)
              case None => "not possible"
            }
        }
    }
  }

  private def convertStringListToIntList(strList: String): List[Int] = strList.drop(1).dropRight(1).split(", ").map(_.toInt).toList

  private def convertIntListToStringList(intList: List[Int]): String = intList.map(_.toString).mkString(",")

  private def findOneOnOneSide(weights: List[Int], weightDiff: Int): Option[Int] = weights.find(_ == weightDiff)

  private def findTwoOnOneSide(weights: List[Int], weightDiff: Int): Option[List[Int]] = findMatch(weights, weightDiff, (a: Int, b: Int) => a + b)

  private def findOneOnEachSide(weights: List[Int], weightDiff: Int): Option[List[Int]] = findMatch(weights, weightDiff, (a: Int, b: Int) => abs(a - b))

  private def findMatch(weights: List[Int], weightDiff: Int, op: (Int, Int) => Int): Option[List[Int]] = {
    val possibleMatches = for (i <- weights; j <- weights if op(i, j) == weightDiff && i != j)
      yield List(i, j).sorted
    possibleMatches.headOption
  }


}
