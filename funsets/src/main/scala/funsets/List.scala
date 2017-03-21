
package funsets.Intset.sc

trait List[+T] {
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
  def prepend[U >: T] (elem: U): List[U] = new Cons(elem, this)
}

class Cons[T] (val head:T, val tail:List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends List[T]{
  def isEmpty = true
  def head:Nothing = throw new NoSuchElementException("Nil.Head")
  def tail:Nothing = throw new NoSuchElementException("Nil.Tail")
}


object single {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
  println(singleton(1))
  println(singleton(true))
}

object test {
  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
}