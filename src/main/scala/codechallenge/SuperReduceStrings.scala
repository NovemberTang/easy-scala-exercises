package codechallenge

object SuperReduceStrings {

  def superReduce(string: String): String = string.foldLeft("")(stringReduce)

  private def stringReduce(acc:String, c: Char): String = if(acc.length > 0 && acc.last == c) acc else acc + c

}
