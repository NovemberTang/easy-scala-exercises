package igot99problems

import scala.annotation.tailrec

//http://aperiodic.net/phil/scala/s-99/
object WorkingWithLists {

  def inputTooShort(minListLength: Int) = {
    minListLength match {
      case 1 => throw new IllegalArgumentException(s"List must contain at least 1 element")
      case _ => throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
    }
    throw new IllegalArgumentException(s"List must contain at least $minListLength elements")
  }

  //p01 Find the last element of a list
  @tailrec
  def last(list: List[Int]): Int = {

    list match {
      case Nil => inputTooShort(1)
      case x :: Nil => x
      case x :: xs => last(list.tail)
    }

  }

  //p02 find the penultimate element of a list
  @tailrec
  def penultimate(list: List[Int]): Int = {



    list match {
      case Nil => inputTooShort(2)
      case x ::Nil =>  inputTooShort(2)
      case x :: y :: Nil => x
      case x :: y :: xs => penultimate(list.tail)
    }

  }

  //p03 find the kth element of a list
  def nth(n: Int, list: List[Int]): Int = {
    require(length(list) >= n, "List must contain at least as many elements as the argument")

    @tailrec
    def listRecurse(acc: Int, n: Int, list: List[Int]): Int = {
      if (acc < n) listRecurse(acc + 1, n, list.tail)
      else list.head
    }

    listRecurse(0, n, list)
  }

  def length(list: List[Int]): Int = {

    @tailrec
    def listRecurse(acc:Int, list: List[Int]): Int = if (list.isEmpty) acc else listRecurse(acc + 1, list.tail)

    listRecurse(0, list)
  }

}
