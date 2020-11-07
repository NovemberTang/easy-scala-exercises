package codechallenge

import scala.math.sqrt

object VowelSquare {

  def findTheVowelSquare(characterMatrix: List[String]): (Int, Int) = {

    val boolMatrix: List[List[Boolean]] = characterMatrix.map(stringToBoolList)
    val truePairIndices: List[Int] = boolMatrix.map(checkForConsecutiveMatches(_, true))
    val xIndices = truePairIndices.distinct.filterNot(_ == -1)

    val squareCoordinates: List[(Int, Int)] = xIndices.map{
      x => val y: Int = checkForConsecutiveMatches(truePairIndices, x)
        (x + 1, y + 1)
    }

    squareCoordinates.minBy(tup => pythDistance(tup._1, tup._2))
  }

  def charToBool(char: Char): Boolean = {
    char match {
      case 'a' | 'e' | 'i' | 'o' | 'u' => true
      case _ => false
    }
  }

  def stringToBoolList(str: String): List[Boolean] = str.map(charToBool).toList

  def checkForConsecutiveMatches(list: List[Any], valueToBeChecked: Any): Int = {
    val pairsToBeChecked = list.sliding(2).toList
    val truePairIndex = pairsToBeChecked.indexOf(List(valueToBeChecked, valueToBeChecked))
    truePairIndex
  }

  def square(int: Int): Int = int*int
  def pythDistance(a:Int, b:Int): Double = sqrt(square(a) + square(b))


}
