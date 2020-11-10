package igot99problems

import scala.annotation.tailrec

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
  def nth[A](n: Int, list: List[A]): A = {
    require(length(list) >= n, "List must contain at least as many elements as the argument")

    @tailrec
    def listRecurse(acc: Int, n: Int, list: List[A]): A = {
      if (acc < n) listRecurse(acc + 1, n, list.tail)
      else list.head
    }

    listRecurse(0, n, list)
  }

  //p04 find the length of a list
  def length[A](list: List[A]): Int = {
    @tailrec
    def listRecurse(acc: Int, list: List[A]): Int = if (list.isEmpty) acc else listRecurse(acc + 1, list.tail)

    listRecurse(0, list)
  }

  //p05 reverse a list
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseRecurse(inputList: List[A], outputList: List[A]): List[A] = {
      inputList match {
        case List() => outputList
        case _ => reverseRecurse(inputList.init, outputList ++ List(inputList.last))
      }
    }

    reverseRecurse(list, List.empty)
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
  def duplicate[A](list: List[A]): List[A] = {

    @tailrec
    def duplicateRecurse(input: List[A], output: List[A] = List.empty): List[A] = {
      input match {
        case Nil => output
        case x :: xs => duplicateRecurse(input.tail, output ++ List(x, x))
      }
    }

    duplicateRecurse(list)

    //val duplicateList = list
    //list.zip(duplicateList).flatMap(tup => List(tup._1, tup._2))
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
  def split[A](index: Int, list: List[A]): (List[A], List[A]) = {

    @tailrec
    def recSplit(originalList: List[A],
                 newList: List[A] = List.empty,
                 acc: Int = 0,
                 index: Int = index): (List[A], List[A]) = {
      val reachedFinalIndex: Boolean = acc == index

      if (reachedFinalIndex) (newList, originalList)
      else recSplit(originalList.tail, newList ++ List(originalList.head), acc + 1)
    }

    throwIfIndexTooLarge(index, list)
    recSplit(list)

  }

  //p18 extract a slice from a list
  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
    throwIfIndexTooLarge(start, list)
    throwIfIndexTooLarge(end, list)

    val bottomSlice = split(end, list)._1
    split(start, bottomSlice)._2
  }

  //p19 rotate a list n places to the left
  def rotate[A](n: Int, list: List[A]): List[A] = {
    val rotationIndex = if (n > 0) n % length(list) else (n % length(list)) + length(list)

    val splitLists = split(rotationIndex, list)
    splitLists._2 ++ splitLists._1
  }

  //p20 remove nth element from the list
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    throwIfIndexTooLarge(n, list)

    val (split1, end) = split(n, list)
    val (loneElement, split2) = split(1, end)
    (split1++split2, loneElement.head)
  }

  private def inputTooShort(minListLength: Int) = {
    minListLength match {
      case 1 => throw new IllegalArgumentException(s"List must contain at least 1 element")
      case _ => throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
    }
    throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
  }

  //p21 insert element at a given position into a list
  def insertAt[A](elem: A, n: Int, list: List[A]): List[A] = {
    throwIfIndexTooLarge(n, list)
    val (a,b) = split(n,list)
    a ++ List(elem) ++ b
  }

  //p22 create a list containing all integers in a given range
  def range(start: Int, end: Int): List[Int] = {
    require(start < end, "Range must be ascending")
    @tailrec
    def rangeRec(currentElem: Int, start: Int, list: List[Int]): List[Int] = {
      val finalElement: Boolean = currentElem == start
      if (finalElement) start :: list else rangeRec(currentElem - 1, start, currentElem :: list)
    }

    rangeRec(end, start, List())
  }
  private def throwIfIndexTooLarge[A](index: Int, list: List[A]): Unit = {
    val indexIsTooLarge = index > length(list)
    if (indexIsTooLarge) throw new IndexOutOfBoundsException("Index cannot be larger than the length of the list")
  }

}