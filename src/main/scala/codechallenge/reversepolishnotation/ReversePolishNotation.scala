package codechallenge.reversepolishnotation

import scala.language.implicitConversions

object ReversePolishNotation {
  implicit def stringToString(string: String): CalcString = new CalcString(string)
}
