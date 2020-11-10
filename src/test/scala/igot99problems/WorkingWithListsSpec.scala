package igot99problems

import WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkingWithListsSpec extends AnyFlatSpec with Matchers {

  private val intList = List(1, 1, 2, 3, 5, 8)
  private val symbolList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

  "last" should "return the last element of that list" in {

    last(intList) shouldEqual 8

  }
  it should "throw an IllegalArgumentException when given an empty list" in {

    intercept[IllegalArgumentException](last(List.empty))

  }

  "penultimate" should "return the last but one element of that list" in {
    penultimate(intList) shouldEqual 5
  }
  it should "throw an IllegalArgumentException if given a list that is too short" in {
    intercept[IllegalArgumentException](penultimate(List.empty))
    intercept[IllegalArgumentException](penultimate(List(0)))
  }

  "nth" should "find the nth element in a list" in {
    nth(2, intList) shouldEqual 2
  }

  "length" should "calculate the length of a list" in {
    WorkingWithLists.length(intList) shouldEqual 6
  }

  "reverse" should "create a list with the original elements in reverse order" in {
    reverse(intList) shouldEqual List(8, 5, 3, 2, 1, 1)
  }

  "isPalindrome" should " return true for a list that is identical in reverse" in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldEqual true
  }
  it should "return false for a list that is not the same forwards and backwards" in {
    isPalindrome(intList) shouldEqual false
  }

  ignore should "flatten a nested list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual intList
  }

  "compress" should "remove consecutive duplicates" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "duplicate" should "duplicate each element of a list" in {
    duplicate(intList) shouldEqual List(1, 1, 1, 1, 2, 2, 3, 3, 5, 5, 8, 8)
  }

  "duplicateN" should "duplicate each element n times" in {
    duplicateN(1, intList) shouldEqual intList
    duplicateN(3, intList) shouldEqual List(1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 5, 5, 5, 8, 8, 8)
  }

  "drop" should "drop every nth element from a list" in {
    drop(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)) shouldEqual List(1, 2, 4, 5, 7, 8)
  }

  "split" should "split a list at the appropriate index" in {
    val actual = split(3, symbolList)
    val expected = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    actual shouldEqual expected
    split(6, intList) shouldEqual(intList, List.empty)
    intercept[IllegalArgumentException](split(7, intList))
  }

  "slice" should "take a slice of a list at the appropriate indices" in {
    slice(3, 7, symbolList) shouldEqual List('d, 'e, 'f, 'g)
  }

  "rotate" should "rotate a list n places to the left" in {
    rotate(3, symbolList) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  }
  it should "handle negative inputs corectly" in {
    rotate(-2, symbolList) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }
}
