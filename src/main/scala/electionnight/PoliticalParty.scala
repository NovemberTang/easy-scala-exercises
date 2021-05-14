package electionnight

abstract sealed class PoliticalParty{
  override def toString: String = this.getClass.getSimpleName.replace("$","")
}

case object Labour       extends PoliticalParty
case object Conservative extends PoliticalParty
case object UKIP         extends PoliticalParty {override def toString = "UK Independence Party"}
case object Green        extends PoliticalParty
case object SNP          extends PoliticalParty {override def toString = "Scottish National Party"}
case object LibDems      extends PoliticalParty {override def toString = "Liberal Democrats"}
case object Independent  extends PoliticalParty


