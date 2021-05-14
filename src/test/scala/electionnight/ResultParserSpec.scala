package electionnight

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ResultParser._

class ResultParserSpec extends AnyFlatSpec with Matchers {

  "A valid constituency" should "convert to a string correctly" in {
    val smallConstituency = new ConstituencyScore("A Constituency", Map("L" -> 13, "C" -> 14, "SNP" -> 45))
    smallConstituency.toString shouldEqual "A Constituency: Labour - 18%, Conservative - 19%, Scottish National Party - 63%"
  }

  "A valid input" should "return the correct success string" in {
    val output = readLineWithErrorHandling("Cardiff West, 11014, C, 17803, L, 4923, UKIP, 2069, LD")
    val expectedOutput = Some("Cardiff West: Conservative - 31%, Labour - 50%, UK Independence Party - 14%, Liberal Democrats - 6%\n")
    output shouldEqual expectedOutput
  }

  "An invalid input" should "fail correctly" in {
    readLineWithErrorHandling("this, won't, work") shouldEqual None
    readLineWithErrorHandling(", , , ") shouldEqual None
    readLineWithErrorHandling("") shouldEqual None
  }

  "A constituency where nobody voted" should "succeed anyway" in {
    val noResidents = new ConstituencyScore("Emptytown", Map.empty)
    noResidents.toString shouldEqual "Emptytown: "
  }

  "A list of strings containing election results" should "be converted into a string describing vote shares for each constituency" in {
    val cons1 = "Cardiff West, 11014, C, 17803, L, 4923, UKIP, 2069, LD"
    val cons2 = "fakeplace, won't, work"
    val cons3 = "Islington South & Finsbury, 22547, L, 9389, C, 4829, LD, 3375, UKIP, 3371, G, 309, Ind"
    val cons4 = ", , , "
    val resultsList = Iterator(cons1, cons2, cons3, cons4)
    val expectedResult = Seq(
      "Cardiff West: Conservative - 31%, Labour - 50%, UK Independence Party - 14%, Liberal Democrats - 6%\n",
      "Islington South & Finsbury: Labour - 51%, Conservative - 21%, Independent - 1%, UK Independence Party - 8%, Green - 8%, Liberal Democrats - 11%\n")
    parseAllResults(resultsList).toSeq shouldEqual expectedResult
  }



}
