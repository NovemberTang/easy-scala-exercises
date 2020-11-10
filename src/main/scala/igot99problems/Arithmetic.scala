package igot99problems

import WorkingWithLists.range
import scala.annotation.tailrec

object Arithmetic {

  //p31 determine whether an integer is prime
  def isPrime(int: Int): Boolean = {

    @tailrec
    def primeRecurse(int: Int, divisors: List[Int]): Boolean = {
      divisors match {
        case List(1) => true
        case _ =>
          val nonPrime = int % divisors.last == 0
          if (nonPrime) false
          else primeRecurse(int, divisors.init)
      }
    }

    int match {
      case 0 => false
      case 1 => false
      case _ => primeRecurse(int, range(1, Math.sqrt(int).toInt))
    }

  }

}
