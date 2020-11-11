package codechallenge.reversepolishnotation

class CalcString(val string: String){

  def calc: Double = {
    val calcList = string.split(" ")
    val operands = calcList.dropRight(1).map(_.toDouble)
    val operator = calcList.last

    operator match {
      case "+" => operands.sum
      case "-" => operands.reduce(_ - _)
      case "/" => operands.reduce(_ / _)
      case "*" => operands.product
    }


  }

}
