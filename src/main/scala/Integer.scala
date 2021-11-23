case class Integer (value:Nat,sign:Sign) extends Nat with Sign {
  def isZero:Boolean=value.isZero

  override def predecessor: Nat ={
    if (isZero)
      Integer(value.successor,Negative)
    else if (sign.isPositive)
      Integer(value.predecessor,sign)
    else Integer(value.successor,Negative)
  }


  override def successor: Nat = {
    if (isZero)
      Integer(value.successor,Positive)
    else if (sign.isPositive)
      Integer(value.successor,sign)
    else  Integer(value.predecessor,Negative)

  }

  override def +(that: Nat): Nat = {
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor
  }

  override def -(that: Nat): Nat = {
    if (that.isZero) this
    else that match{
      case Integer(value, sign) =>this + Integer(value,sign.negate)
    }
  }

  def isPositive:Boolean=sign.isPositive

  def negate:Integer= Integer(value,sign.negate)

}
