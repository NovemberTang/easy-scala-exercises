package electionnight

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ResultParser._

class ResultParserSpec extends AnyFlatSpec with Matchers {

  "A valid constituency" should "convert to a string correctly" in {
    val smallConstituency = new ConstituencyScore("A Constituency", Map("L" -> 13, "C" -> 14, "SNP" -> 45))
    smallConstituency.toString shouldEqual "A Constituency: Labour - 18%, Conservative - 19%, Scottish National Party - 63%"
  }

  "A valid input" should "correctly convert into a case class" in {
    val cons1 = lineToConstituencyScore("Cardiff West, 11014, C, 17803, L, 4923, UKIP, 2069, LD")
    val cons2 = lineToConstituencyScore("Islington South & Finsbury, 22547, L, 9389, C, 4829, LD, 3375, UKIP, 3371, G, 309, Ind")

    val expectedResult1 = new ConstituencyScore(
      constituency = "Cardiff West",
      voteMap = Map("C" -> 11014, "L" -> 17803, "UKIP" -> 4923, "LD" -> 2069))
    val expectedResult2 = new ConstituencyScore(
      constituency = "Islington South & Finsbury",
      voteMap = Map("LD" -> 4829, "Ind" -> 309, "G" -> 3371, "L" -> 22547, "C" -> 9389, "UKIP" -> 3375))

    assert(cons1 equals expectedResult1)
    assert(cons2 equals expectedResult2)

  }
  it should "return the correct success string" in {
    val output = readLineWithErrorHandling("Cardiff West, 11014, C, 17803, L, 4923, UKIP, 2069, LD")
    val expectedOutput = "Cardiff West: Conservative - 31%, Labour - 50%, UK Independence Party - 14%, Liberal Democrats - 6%"
    output shouldEqual expectedOutput
  }

  "An invalid input" should "fail correctly" in {
    readLineWithErrorHandling("this, won't, work") shouldEqual "Failed to get data for <this>"
    readLineWithErrorHandling(", , , ") shouldEqual "Failed to get data for an unknown constituency"
  }

  "A constituency where nobody voted" should "succeed anyway" in {
    val noResidents = new ConstituencyScore("Emptytown", Map.empty)
    noResidents.toString shouldEqual "Emptytown: "
  }

}
