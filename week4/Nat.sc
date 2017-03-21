
package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat =  new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("No previous element")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) that else throw new Error("No previous element")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat) = new Succ(n) + that
  def - (that: Nat) = new Succ(n) - that
}
