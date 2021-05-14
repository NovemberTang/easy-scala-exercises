package electionnight

class ConstituencyScore(val constituency: String,
                        val voteMap: Map[String,Int]
                       ) {
  def equals(that: ConstituencyScore): Boolean = this.constituency == that.constituency && this.voteMap == that.voteMap

  private def percentage(code: String): Int = ((voteMap(code).toDouble / voteMap.values.sum) * 100).round.toInt

  private def codeToFullName(code: String): PoliticalParty = {
    //will throw an exception if there is an invalid political party
    code match {
      case "L" => Labour
      case "C" => Conservative
      case "UKIP" => UKIP
      case "G" => Green
      case "LD" => LibDems
      case "SNP" => SNP
      case "Ind" => Independent
    }
  }

  val percentageMap: Map[PoliticalParty, Int] = voteMap.map{ x => val party = x._1
    (codeToFullName(party), percentage(party))
  }

  override def toString: String = s"$constituency: " + percentageMap.map{x =>
  val (party, percentage) = x
  s"$party - $percentage%"}.mkString(", ")
}
