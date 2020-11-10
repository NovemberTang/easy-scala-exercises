package igot99problems

import scala.annotation.tailrec
import scala.util.Random

//http://aperiodic.net/phil/scala/s-99/
object WorkingWithLists {

  //p01 Find the last element of a list
  @tailrec
  def last[A](list: List[A]): A = {
    list match {
      case Nil => inputTooShort(1)
      case x :: Nil => x
      case x :: xs => last(list.tail)
    }
  }

  //p02 find the penultimate element of a list
  @tailrec
  def penultimate[A](list: List[A]): A = {

    list match {
      case Nil => inputTooShort(2)
      case x :: Nil => inputTooShort(2)
      case x :: y :: Nil => x
      case x :: y :: xs => penultimate(list.tail)
    }

  }

  //p03 find the kth element of a list
  @tailrec
  def nth[A](n: Int, list: List[A], acc: Int = 0): A =
    if (acc < n) nth(n, list.tail, acc + 1)
    else list.head

  //p04 find the length of a list
  @tailrec
  def length[A](list: List[A], acc: Int = 0): Int = {
    if (list.isEmpty) acc else length(list.tail, acc + 1)
  }

  //p05 reverse a list
  @tailrec
  def reverse[A](inputList: List[A], outputList: List[A] = List.empty): List[A] = {
    inputList match {
      case List() => outputList
      case _ => reverse(inputList.init, outputList ++ List(inputList.last))
    }
  }

  //p06 is a list a palindrome
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)

  //p07 flatten nested lists
  def flatten(list: List[Any]): List[Int] = ???

  //p08 eliminate consecutive duplicates
  def compress[A](list: List[A]): List[A] = {
    list match {
      case List() => List()
      case x :: xs => List(x) ++ compress(xs.dropWhile(_ == x))
    }
  }

  //p14 duplicate elements of a list
  @tailrec
  def duplicate[A](input: List[A], output: List[A] = List.empty): List[A] = {
    input match {
      case Nil => output
      case x :: xs => duplicate(input.tail, output ++ List(x, x))
    }
  }

  //p15 duplicate list elements a given number of times
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    def createDuplicatesOfELement(elem: A, n: Int = n, input: List[A] = List.empty): List[A] = {
      n match {
        case 0 => input
        case _ => elem :: createDuplicatesOfELement(elem, n - 1, input)
      }
    }

    n match {
      case 0 => inputTooShort(1)
      case 1 => list
      case _ => list.flatMap(createDuplicatesOfELement(_))
    }
  }

  //p16 drop every nth element from a list
  def drop[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def dropRec(acc: Int,
                input: List[A] = list,
                outputOfLastCall: List[A] = List.empty,
                n: Int = n): List[A] = {

      val indexIsMultipleOfN: Boolean = acc % n == 0

      input match {
        case Nil => outputOfLastCall
        case _ =>
          if (indexIsMultipleOfN) dropRec(acc + 1, input.tail, outputOfLastCall)
          else dropRec(acc + 1, input.tail, outputOfLastCall ++ List(input.head))
      }
    }

    dropRec(1)
  }

  //p17 split a list into two parts
  @tailrec
  def split[A](originalList: List[A], index: Int, acc: Int = 0, newList: List[A] = List.empty): (List[A], List[A]) = {

    val reachedFinalIndex: Boolean = acc == index

    if (reachedFinalIndex) (newList, originalList)
    else split(originalList.tail, index, acc = acc + 1, newList = newList ++ List(originalList.head))


  }

  //p18 extract a slice from a list
  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
    throwIfIndexTooLarge(start, list)
    throwIfIndexTooLarge(end, list)

    val bottomSlice = split(list, end)._1
    split(bottomSlice, start)._2
  }

  //p19 rotate a list n places to the left
  def rotate[A](n: Int, list: List[A]): List[A] = {
    val rotationIndex = if (n > 0) n % length(list) else (n % length(list)) + length(list)

    val splitLists = split(list, rotationIndex)
    splitLists._2 ++ splitLists._1
  }

  //p20 remove nth element from the list
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    throwIfIndexTooLarge(n, list)

    val (split1, end) = split(list, n)
    val (loneElement, split2) = split(end, 1)
    (split1 ++ split2, loneElement.head)
  }

  //p21 insert element at a given position into a list
  def insertAt[A](elem: A, n: Int, list: List[A]): List[A] = {
    throwIfIndexTooLarge(n, list)
    val (a, b) = split(list, n)
    a ++ List(elem) ++ b
  }

  //p22 create a list containing all integers in a given range
  @tailrec
  def range(start: Int, end: Int, list: List[Int] = List.empty): List[Int] = {
    require(start <= end, "Range must be ascending")
    val finalElement: Boolean = end == start
    if (finalElement) start :: list else range(start, end - 1, end :: list)
  }

  //p23 Extract a given number of randomly selected elements from a list.
  @tailrec
  def randomSelect[A](n: Int, inlist: List[A], outlist: List[A] = List.empty): List[A] = {

    def addElementToRandomList(inlist: List[A], outlist: List[A]) = {
      val rand = scala.util.Random
      val listLength: Int = length(inlist)
      val randomIndex = rand.nextInt(listLength)
      val randomListElem = nth(randomIndex, inlist)
      randomListElem :: outlist
    }

    val updatedRandomList = addElementToRandomList(inlist, outlist)

    n match {
      case 0 => outlist
      case _ =>
        randomSelect(n - 1, inlist, updatedRandomList)
    }
  }

  private def inputTooShort(minListLength: Int) = {
    minListLength match {
      case 1 => throw new IllegalArgumentException(s"List must contain at least 1 element")
      case _ => throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
    }
    throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
  }

  private def throwIfIndexTooLarge[A](index: Int, list: List[A]): Unit = {
    val indexIsTooLarge = index > length(list)
    if (indexIsTooLarge) throw new IndexOutOfBoundsException("Index cannot be larger than the length of the list")
  }

}