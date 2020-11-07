package codechallenge

import codechallenge.WeightBalancing._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WeightBalancingSpec extends AnyFlatSpec with Matchers {

  "scales" should "be properly balanced" in {

    balanceTheWeight("[3, 7]", "[1, 2, 7]") shouldBe "not possible"
    balanceTheWeight("[5, 9]", "[1, 2, 6, 7]") shouldBe "2,6"
    balanceTheWeight("[13, 4]", "[1, 2, 3, 6, 14]") shouldBe "3,6"
    balanceTheWeight("[3, 4]", "[1, 2, 7, 7]") shouldBe "1"
    balanceTheWeight("[1, 5]", "[1, 11, 7, 5]") shouldBe "1,5" //(not "7, 11")

  }

}
