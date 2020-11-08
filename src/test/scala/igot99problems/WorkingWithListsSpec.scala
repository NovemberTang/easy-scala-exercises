package igot99problems

import WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkingWithListsSpec extends AnyFlatSpec with Matchers {

  private val exampleList = List(1, 1, 2, 3, 5, 8)
  "last" should "return the last element of that list" in {

    last(exampleList) shouldEqual 8

  }
  it should "throw an IllegalArgumentException when given an empty list" in {

    intercept[IllegalArgumentException](last(List.empty))

  }

  "penultimate" should "return the last but one element of that list" in {

    penultimate(exampleList) shouldEqual 5

  }
  it should "throw an IllegalArgumentException if given a list that is too short" in {

    intercept[IllegalArgumentException](penultimate(List.empty))
    intercept[IllegalArgumentException](penultimate(List(0)))

  }

  "nth" should "find the nth element in a list" in {

    nth(2, exampleList) shouldEqual 2

  }

  "length" should "calculate the length of a list" in {
    WorkingWithLists.length(exampleList) shouldEqual 6
  }

  "reverse" should "create a list with the original elements in reverse order" in {

    reverse(exampleList) shouldEqual List(8, 5, 3, 2, 1, 1)

  }

  "isPalindrome" should " return true for a list that is identical in reverse" in {

    isPalindrome(List(1,2,3,2,1)) shouldEqual true

  }
  it should "return false for a list that is not the same forwards and backwards" in {
    isPalindrome(exampleList) shouldEqual false
  }

}
