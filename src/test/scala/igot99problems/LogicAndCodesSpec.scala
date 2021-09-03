package igot99problems

import LogicAndCodes._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogicAndCodesSpec extends AnyFlatSpec with Matchers {

  "logical and" should "return true iff both inputs are true" in {

    and(true, true) shouldBe true
    and(true, false) shouldBe false
    and(false, true) shouldBe false
    and(false, false) shouldBe false
  }

  "logical or" should "return true if any input is true" in {
    or(true, true) shouldBe true
    or(true, false) shouldBe true
    or(false, true) shouldBe true
    or(false, false) shouldBe false
  }

  "logical not" should "flip a boolean" in {
    LogicAndCodes.not(true) shouldBe false
    LogicAndCodes.not(false) shouldBe true
  }

  "logical nand" should "return false iff both inputs are true" in {
    nand(true, true) shouldBe false
    nand(true, false) shouldBe true
    nand(false, true) shouldBe true
    nand(false, false) shouldBe true
  }

  "logical nor" should "return false if any input is true" in {
    nor(true, true) shouldBe false
    nor(true, false) shouldBe false
    nor(false, true) shouldBe false
    nor(false, false) shouldBe true
  }

  "equ" should "return true if both inputs are the same" in {
    equ(true, true) shouldBe true
    equ(true, false) shouldBe false
    equ(false, true) shouldBe false
    equ(false, false) shouldBe true
  }

  "xor" should "return true if one input is true and the other is false" in {
    xor(true, true) shouldBe false
    xor(true, false) shouldBe true
    xor(false, true) shouldBe true
    xor(false, false) shouldBe false
  }
}
