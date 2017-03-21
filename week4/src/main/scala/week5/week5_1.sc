

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))
removeAt(5, List('a', 'b', 'c', 'd'))

val xt = List('a', 'b', 'c', 'd','e','f')

xt take 3

xt drop 3


def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys =>
}

