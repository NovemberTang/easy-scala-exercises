package igot99problems

import Arithmetic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticSpec extends AnyFlatSpec with Matchers {

  "prime" should "correctly identify prime numbers" in {
    isPrime(7) shouldBe true
    isPrime(2) shouldBe true
    isPrime(1) shouldBe false
  }

}
