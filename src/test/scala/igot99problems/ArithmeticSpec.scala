package igot99problems

import Arithmetic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticSpec extends AnyFlatSpec with Matchers {

  "prime" should "correctly identify prime numbers" in {
    isPrime(7) shouldBe true
    isPrime(2) shouldBe true
    isPrime(1) shouldBe false
    isPrime(8) shouldBe false
  }

  "gcd" should "find the highest common divisor of two numbers" in{
    gcd(36, 63) shouldEqual 9
    gcd(1, 4) shouldEqual 1
  }

  "an integer" should "be able to work out whether another integer is coprime with it" in {
    3.isCoprimeTo(1) shouldBe true
    3.isCoprimeTo(36) shouldBe false
    36.isCoprimeTo(3) shouldBe false
    8.isCoprimeTo(64) shouldBe false
  }

  "all prime factors" should "be returned" in {
    315.primeFactors shouldBe List(3, 3, 5, 7)
  }

}
