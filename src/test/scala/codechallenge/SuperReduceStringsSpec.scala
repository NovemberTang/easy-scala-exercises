package codechallenge

import SuperReduceStrings.superReduce
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SuperReduceStringsSpec extends AnyFlatSpec with Matchers {

  "strings with consecutive duplicates" should "have those duplicates removed" in {
    superReduce("aaaabccaa") shouldBe "abca"
  }

}
