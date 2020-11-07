package codechallenge

import codechallenge.VowelSquare.findTheVowelSquare
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class VowelSquareSpec extends AnyFlatSpec with Matchers {

  val input = List("abcd", "eikr", "oufj")

  "Questionmarks" should "detect exactly 3 question marks between digits that add up to 10" in {
    findTheVowelSquare(input) shouldEqual (1,2)
  }

}
