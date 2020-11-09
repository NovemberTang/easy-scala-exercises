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
  def compress(string: List[Any]): List[Any] = {
    def removeDuplicates(acc: List[Any], c: Any): List[Any] = if (acc.nonEmpty && acc.last == c) acc else acc ++ List(c)
    val x: Any = ""

    string.foldLeft(List(x))(removeDuplicates).tail
  }

  private def inputTooShort(minListLength: Int) = {
    minListLength match {
      case 1 => throw new IllegalArgumentException(s"List must contain at least 1 element")
      case _ => throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
    }
    throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
  }

}