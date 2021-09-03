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

//Add test for problem 34

  "all prime factors" should "be returned" in {
    315.primeFactors shouldBe List(3, 3, 5, 7)
  }

  "all prime factors" should "be returned with their counts" in {
    315.primeFactorMultiplicity shouldBe List((3, 2), (5, 1), (7,1))
  }

  "Prime lists" should "work correctly" in {
    listPrimesinRange(7 to 31) shouldBe List(7, 11, 13, 17, 19, 23, 29, 31)
  }

  "Goldbach pairs" should "be generated correctly for an even number" in {
    28.goldbach shouldBe (5, 23)
  }

  "Goldbach compositions" should "be produced given a valid range" in {
    createGoldbackStrings(9 to 20) shouldBe
      List("10 = 3 + 7", "12 = 5 + 7", "14 = 3 + 11", "16 = 3 + 13", "18 = 5 + 13", "20 = 3 + 17")
  }

}
