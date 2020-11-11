package codechallenge

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import codechallenge.reversepolishnotation.ReversePolishNotation._

class ReversePolishNotationSpec extends AnyFlatSpec with Matchers {
  "calc" should "be able to calculate single digit numbers" in {
    "1 2 +".calc should equal(3)
  }

  it should "be able to calculate multi digit numbers" in {
    "12 3 /".calc should equal(4)
  }

  it should "be able to handle negative numbers" in {
    "-12 3 /".calc should equal(-4)
  }

  it should "be able to handle decimal numbers" in {
    "-12.9 3 /".calc should equal(-4.3)
  }

  it should "be able to calculate more complex notations" in {
    "1 2 + 4 * 5 + 3 -".calc should equal(14)
  }

}
