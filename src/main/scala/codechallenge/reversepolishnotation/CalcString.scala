package codechallenge.reversepolishnotation

import scala.annotation.tailrec

class CalcString(val string: String) {

  def calc: Double = {
    @tailrec
    def calcRecurse(string: String): String = {
      string.last match {
        case '+' | '-' | '*' | '/' =>
          val (result, charsToDrop) = buildString(string)
          val newString = result + string.drop(charsToDrop)
          calcRecurse(newString)
        case _ => string
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
    val newOutput = outputString + inputString.head
    val notAValidCalculation = newOutput.split(" ").length < 3

    inputString.head match {
      case '+' | '-' | '*' | '/' =>
        if (notAValidCalculation) buildString(inputString.tail, newOutput)
        else {
          val operation = newOutput
          val result = calculate(operation)
          val opLength = operation.length
          (result, opLength)
        }
      case _ => buildString(inputString.tail, newOutput)
    }
  }

}
