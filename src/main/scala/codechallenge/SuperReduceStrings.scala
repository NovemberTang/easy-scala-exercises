package codechallenge

object SuperReduceStrings {

  def superReduce(string: String): String = {

    def stringReduce(acc:String, c: Char): String = {
      if(acc.length > 0 && acc.last == c) acc else acc + c
    }

    string.foldLeft("")(stringReduce)

  }

}
