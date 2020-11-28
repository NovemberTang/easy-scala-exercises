package igot99problems

import WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkingWithListsSpec extends AnyFlatSpec with Matchers {

  private val intList = List(1, 1, 2, 3, 5, 8)
  private val symbolList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
  private val charList = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  private val longSymbolList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


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

  "flatten" should "flatten a nested list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual intList
  }

  "compress" should "remove consecutive duplicates" in {
    compress(longSymbolList) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "pack" should "put duplicate elements into sublists" in {
    val outputList = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    pack(longSymbolList) shouldEqual outputList
  }

  "encode" should "show the run length encoding of a packed list" in {
    encode(longSymbolList) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "encodeModified" should "return single elements as themselves" in {
    encodeModified(longSymbolList) shouldEqual List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "decode" should "decode a run length encoded list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldEqual longSymbolList
  }

  "encodeDirect" should "show the run length encoding of a packed list" in {
    encodeDirect(longSymbolList) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
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
    val actual = split(symbolList, 3)
    val expected = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    actual shouldEqual expected
    split(intList, 6) shouldEqual(intList, List.empty)
  }

  "slice" should "take a slice of a list at the appropriate indices" in {
    slice(3, 7, symbolList) shouldEqual List('d, 'e, 'f, 'g)
  }

  "rotate" should "rotate a list n places to the left" in {
    rotate(3, charList) shouldEqual List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
  }
  it should "handle negative inputs corectly" in {
    rotate(-2, charList) shouldEqual List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
  }

  "removeAt" should "remove one element from the nth position of the list" in {
    removeAt(1, List('a', 'b', 'c', 'd')) shouldEqual ((List('a', 'c', 'd'),'b'))
    removeAt(3, List('a', 'b', 'c', 'd')) shouldEqual ((List('a', 'b', 'c'), 'd'))
  }

  "insertAt" should "correctly place a new element into a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }

  "range" should "create a list of ints from start to end" in {
    range(4, 9) shouldEqual List(4, 5, 6, 7, 8, 9)
  }
  it should "throw an exception if given invalid arguments" in {
    intercept[IllegalArgumentException](range(3,0))
  }

  "randomSelect" should "select n random elements from a list" in {
    val output = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    output should have length 3
  }

  "lotto" should "draw n different elements from a range" in {
    lotto(6, 49) should have length(6)
  }

  "randomPermute" should "return all list elements in a random order" in {
    val input = List('a', 'b', 'c', 'd', 'e', 'f')
    val output = randomPermute(input)
    output should contain theSameElementsAs input
  }

  "combinations" should "return the list of all possible combinations of the elements" in {
    val actualOutput = combinations(3, List(1,2,3,4,5,6,7,8,9,10,11,12))
    actualOutput.length shouldEqual(220)
  }

  "lsort" should "sort a list of lists by length of sublist" in {
    val input = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val output = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    lsort(input) shouldEqual output
  }
}
