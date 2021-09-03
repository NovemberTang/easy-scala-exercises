package igot99problems

import scala.annotation.tailrec
import scala.language.implicitConversions

object Arithmetic {

  implicit def intToInteger(int: Int): Integer = new Integer(int)

  //p31 determine whether an integer is prime
  def isPrime(int: Int): Boolean = {

    @tailrec
    def primeRecurse(int: Int, factors: LazyList[Int]): Boolean = {
      factors match {
        case LazyList(1) => true
        case _ =>
          val nonPrime = int % factors.head == 0
          if (nonPrime) false else primeRecurse(int, factors.tail)
      }
    }

    val possibleFactors = createDescLazyList(Math.sqrt(int).toInt, 0)
    if(int == 1) false else primeRecurse(int, possibleFactors)
  }

  //p32 find greatest common divisor of two positive integers
  def gcd(a: Int, b: Int): Int = {
    val largestPossible = if (a < b) a else b
    val possibleAnswers = createDescLazyList(largestPossible, 1)

    @tailrec
    def findGcd(stream: LazyList[Int], a: Int, b: Int): Int = {
      val streamHeadDividesBoth: Boolean = a % stream.head == 0 && b % stream.head == 0
      if (streamHeadDividesBoth) stream.head else findGcd(stream.tail, a, b)
    }

    if (a == 1 || b == 1) 1 else findGcd(possibleAnswers, a, b)
  }

  private def createDescLazyList(hi: Int, lo: Int): LazyList[Int] = {
    if (lo >= hi) LazyList.empty
    else hi #:: createDescLazyList(hi - 1, lo)
  }

  class Integer(value: Int){
    //p33 determine whether two positive integers are coprime
    def isCoprimeTo(int: Int): Boolean = gcd(value,int) == 1

    //p34 Calculate Euler's totient
    def totient: Int = 1.to(value).count(_.isCoprimeTo(value))

    //35 Find the prime factors of a number
    def primeFactors: List[Int] = {
      @tailrec
      def findPrimeFactors(value: Int, factors: List[Int] = List.empty): List[Int] = {
        val largestPrime = (value/2).to(2).by(-1).find(x => isPrime(x) && value%x == 0).getOrElse(value)
        if (value/largestPrime==1) largestPrime :: factors
        else findPrimeFactors(value/largestPrime, largestPrime :: factors)
      }
      findPrimeFactors(value)
    }

  }


}

