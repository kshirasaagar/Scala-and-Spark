import funsets.{Rational,rationals}
import funsets.List

abstract class IntSet{
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  def union(other:IntSet):IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x) else this

  def union(other:IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"

}


val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4
val t3 = t2 incl 5
val t4 = t3 incl 2
val t5 = t4 incl 3
val t6 = t5 incl 0
val t7 = t6 incl 1

val t8 = t7 contains 4

val t9 = t7 union t6

val s1 = new funsets.Rational(1,3)


def f(xs: List[NonEmpty], x: Empty) = xs prepend x