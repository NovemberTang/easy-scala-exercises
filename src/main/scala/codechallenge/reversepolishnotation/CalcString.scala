package codechallenge.reversepolishnotation

import scala.annotation.tailrec

class CalcString(val string: String) {

  def calc: Double = {

    @tailrec
    def calcRecurse(string: String): String = {
      string.length match {
        case 1 => string
        case _ => {
          val (result, charsToDrop) = buildString(string)
          val newString = result + string.drop(charsToDrop)
          calcRecurse(newString)
        }
      }
    }

    calcRecurse(string).toDouble

  }


  private def calculate(string: String): Double = {
    val calculationCharList = string.split(" ")
    val operands = calculationCharList.dropRight(1).map(_.toDouble)
    val operator = calculationCharList.last

    operator match {
      case "+" => operands.sum
      case "-" => operands.reduce(_ - _)
      case "/" => operands.reduce(_ / _)
      case "*" => operands.product
    }

  }

  @tailrec
  private def buildString(inputString: String, outputString: String = ""): (Double, Int) = {

    inputString.head match {
      case '+' | '-' | '*' | '/' => {
        val operation = (outputString + inputString.head)
        val result = calculate(operation)
        val opLength = operation.length
        (result, opLength)
      }
      case char => buildString(inputString.tail, s"$outputString${char}")
    }

  }

}
