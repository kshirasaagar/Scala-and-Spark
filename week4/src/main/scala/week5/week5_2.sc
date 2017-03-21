
import math.Ordering

object mergesort{
  def msort[T](xs:List[T]) (implicit ord:Ordering[T]) : List[T] = {
    val n = xs.length/2
    if(n==0) xs
    else{
      def merge(xs:List[T],ys:List[T]):List[T] = (xs,ys) match {
        case (Nil,ys) => ys
        case (xs,Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x,y)) x :: merge(xs1,ys) else y :: merge(xs,ys1)
      }
    val (fst,snd) = xs splitAt n
    merge(msort(fst),msort(snd))
    }
  }

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y * y) :: squareList(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if(y > 0) y :: posElems(ys) else posElems(ys)
  }

  def posElems2(xs:List[Int]) : List[Int] = xs filter (_ > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val z = xs span (y => y == x)
      z._1 :: pack(z._2)
  }


  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(x => f(x))

  pack(xs1)

  val xs1 = List(3,2,1,0,3,2,1,0,3,2)
  msort(xs1)

  val xs2 = List("aas","asds","asdasda","a","as")
  msort(xs2)

  val xs3 = List(3,4,-2,1,0,-2,3,-4)
  squareList(xs1)
  squareList2(xs1)
  posElems(xs3)
  posElems2(xs3)

  xs3 filter (x => x > 0)
  xs3 filterNot (x => x > 0)
  xs3 partition (x => x > 0)
  xs3 takeWhile (x => x > 0)
  xs3 dropWhile (x => x > 0)
}



