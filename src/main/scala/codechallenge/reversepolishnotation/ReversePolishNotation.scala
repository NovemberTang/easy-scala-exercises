package codechallenge.reversepolishnotation

object ReversePolishNotation {
  implicit def stringToString(string: String): CalcString = new CalcString(string)
}
