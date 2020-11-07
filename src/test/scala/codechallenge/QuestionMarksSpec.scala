package codechallenge

import codechallenge.QuestionMarks.checkAgainstConditions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuestionMarksSpec extends AnyFlatSpec with Matchers {

  "Questionmarks" should "detect exactly 3 question marks between digits that add up to 10" in {
    checkAgainstConditions("arrb6???4xxbl5???eee5") shouldBe true
    checkAgainstConditions("acc?7??sss?3rr1??????5") shouldBe true
    checkAgainstConditions("9???1???9???1???9") shouldBe true
    checkAgainstConditions("aa6?9") shouldBe false
  }

}
