package igot99problems

object LogicAndCodes {

  def and(b1:Boolean, b2: Boolean): Boolean = {
    (b1,b2) match {
      case (true, true) => true
      case _ => false
    }
  }

def or(b1: Boolean, b2: Boolean): Boolean = {
  (b1, b2) match {
    case (false, false) => false
    case _ => true
  }
}

  def not(b: Boolean) = {
    b match {
      case true => false
      case false => true
    }
  }

  def nand(b1:Boolean, b2: Boolean): Boolean = not(and(b1, b2))

  def nor(b1:Boolean, b2: Boolean): Boolean = not(or(b1, b2))

  def equ(b1:Boolean, b2: Boolean): Boolean = or(and(b1, b2), and(not(b1), not(b2)))

  def xor(b1:Boolean, b2: Boolean): Boolean = not(equ(b1, b2))

  //TODO implement impl and write tests
}
