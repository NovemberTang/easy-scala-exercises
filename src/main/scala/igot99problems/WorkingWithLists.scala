package igot99problems

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Random

//http://aperiodic.net/phil/scala/s-99/
object WorkingWithLists {

  //p01 Find the last element of a list
  @tailrec
  def last[A](list: List[A]): A = {
    throwIfListTooShort(1, list)

    list match {
      case x :: Nil => x
      case _ => last(list.tail)
    }
  }

  //p02 find the penultimate element of a list
  @tailrec
  def penultimate[A](list: List[A]): A = {
    throwIfListTooShort(2, list)
    list match {
      case x :: y :: Nil => x
      case _ => penultimate(list.tail)
    }

  }

  //p03 find the kth element of a list
  @tailrec
  def nth[A](n: Int, list: List[A], acc: Int = 0): A = if (acc < n) nth(n, list.tail, acc + 1) else list.head

  //p04 find the length of a list
  @tailrec
  def length[A](list: List[A], acc: Int = 0): Int = if (list.isEmpty) acc else length(list.tail, acc + 1)

  //p05 reverse a list
  @tailrec
  def reverse[A](inputList: List[A], outputList: List[A] = List.empty): List[A] = {
    inputList match {
      case List() => outputList
      case _ => reverse(inputList.init, outputList :+ inputList.last)
    }
  }

  //p06 is a list a palindrome
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)

  //p07 flatten nested lists
  def flatten(list: List[Any]): List[Any] = {
    list.flatMap {
      case sublist: List[_] => flatten(sublist)
      case elem => List(elem)
    }
  }

  //p08 eliminate consecutive duplicates
  def compress[A](list: List[A]): List[A] = pack(list).map(_.head)

  //p09 pack consecutive duplicates into sublists
  @tailrec
  def pack[A](inputList: List[A], outputList: List[List[A]] = List.empty): List[List[A]] = {
    inputList match {
      case Nil => outputList
      case _ =>
        val (newOutput, newInput) = inputList.span(_ == inputList.head)
        pack(newInput, outputList :+ newOutput)
    }
  }

  //p10 run length encoding of a list
  def encode[A](input: List[A]): List[(Int, A)] = {
    throwIfListTooShort(1, input)
    val packedList = pack(input)
    packedList.map(sublist => (length(sublist), sublist.head))
  }

  //p11 modify the result of problem P10 in such a way that if an element has no duplicates
  // it is simply copied into the result list.
  // Only elements with duplicates are transferred as (N, E) terms.
  def encodeModified[A](list: List[A]): List[Any] = {
    def convertTupleToInt(tuple: (Int, A)): Any = if (tuple._1 == 1) tuple._2 else tuple
    encode(list).map(convertTupleToInt)
  }

  //p12 decode a run length encoded list
  def decode[A](list: List[(Int, A)]): List[A] = {
    def recreateSublist(runLengthTuple: (Int, A)): List[A] = duplicateN(runLengthTuple._1, List(runLengthTuple._2))
    list.flatMap(recreateSublist)
  }

  //p13 implement encode directly
  @tailrec
  def encodeDirect[A](inputList: List[A], outputList: List[(Int, A)] = List.empty): List[(Int, A)] = {
    inputList match {
      case Nil => outputList
      case _ =>
        val (sublist, newInput): (List[A], List[A]) = inputList.span(_ == inputList.head)
        val newOutput: (Int, A) = if (sublist.length == 1) (1, sublist.head) else (sublist.length, sublist.head)
        encodeDirect(newInput, outputList :+ newOutput)
    }
  }

  //p14 duplicate elements of a list
  @tailrec
  def duplicate[A](input: List[A], output: List[A] = List.empty): List[A] = {
    if (input.isEmpty) output
    else duplicate(input.tail, output :+ input.head :+ input.head)
  }

  //p15 duplicate list elements a given number of times
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    throwIfListTooShort(1, list)

    def createDuplicatesOfElement(elem: A, n: Int = n, input: List[A] = List.empty): List[A] = {
      //replicating List.fill(n)(elem)
      n match {
        case 0 => input
        case _ => elem :: createDuplicatesOfElement(elem, n - 1, input)
      }
    }

    n match {
      case 1 => list
      case _ => list.flatMap(createDuplicatesOfElement(_))
    }
  }

  //p16 drop every nth element from a list
  def drop[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def dropRec(n: Int, input: List[A], outputOfLastCall: List[A] = List.empty, acc: Int = 1): List[A] = {
      val indexIsMultipleOfN: Boolean = acc % n == 0

      input match {
        case Nil => outputOfLastCall
        case _ =>
          if (indexIsMultipleOfN) dropRec(n, input.tail, outputOfLastCall, acc + 1)
          else dropRec(n, input.tail, outputOfLastCall :+ input.head, acc + 1)
      }
    }
    dropRec(n, list)
  }

  //p17 split a list into two parts
  @tailrec
  def split[A](originalList: List[A], index: Int, acc: Int = 0, newList: List[A] = List.empty): (List[A], List[A]) = {
    val reachedFinalIndex: Boolean = acc == index
    if (reachedFinalIndex) (newList, originalList)
    else split(originalList.tail, index, acc = acc + 1, newList = newList :+ originalList.head)
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
  def randomSelect[A](n: Int, inputList: List[A], outputList: List[A] = List.empty): List[A] = {
    val updatedRandomList = addRandomToOutputList(inputList, outputList)._1

    n match {
      case 0 => outputList
      case _ =>
        randomSelect(n - 1, inputList, updatedRandomList)
    }
  }

  //p24 Lotto: Draw N different random numbers from the set 1..M.
  def lotto(numberOfElements: Int, rangeMax: Int): List[Int] = {
    val newRange = range(1, rangeMax)
    randomSelect(numberOfElements, newRange)
  }

  //p25 create a random permutation of the elements of a list
  @tailrec
  def randomPermute[A](inputList: List[A], outputList: List[A] = List.empty): List[A] = {
    if (inputList.isEmpty) outputList
    else {
      val (output, randomIndex) = addRandomToOutputList(inputList, outputList)
      val input = removeAt(randomIndex, inputList)._1
      randomPermute(input, output)
    }
  }

  //p26 generate combinations of n distinct objects from a list
  //TODO generalise this function
  def combinations(n: Int, list: List[Int]): List[List[Int]] = {
    throwIfListTooShort(n, list)

    val output: List[List[Int]] = for (i <- list;
                                       j <- list if i != j;
                                       k <- list if i != k && j != k)
      yield List(i, j, k).sorted

    output.sorted.distinct

  }

  //p27

  //p28 sort list of lists by sublist length
  //TODO part B
  def lsort[A](list: List[List[A]]): List[List[A]] = {
    val lengthList = list.map(sublist => length(sublist))
    val sortedZipList: List[(Int, List[A])] = lengthList.zip(list).sortBy(_._1)
    sortedZipList.map(_._2)
  }

  private def addRandomToOutputList[A](inputList: List[A], outputList: List[A]): (List[A], Int) = {
    val rand = Random
    val listLength: Int = length(inputList)
    val randomIndex = rand.nextInt(listLength)
    val randomListElem = nth(randomIndex, inputList)
    (randomListElem :: outputList, randomIndex)
  }

  private def throwIfListTooShort[A](minListLength: Int, list: List[A]): Unit = {
    val listIsTooShort: Boolean = length(list) < minListLength

    if (listIsTooShort) {
      minListLength match {
        case 1 => throw new IllegalArgumentException(s"List must contain at least 1 element")
        case _ => throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
      }
    }
  }

  private def throwIfIndexTooLarge[A](index: Int, list: List[A]): Unit = {
    val indexIsTooLarge = index > length(list)
    if (indexIsTooLarge) throw new IndexOutOfBoundsException("Index cannot be larger than the length of the list")
  }

}