
import math.abs

object exercise{
  def product (f: Int => Int) (a:Int, b:Int): Int = {
    if(a > b) 1 else f(a) * product(f) (a+1,b)
  }
  product(x => x) (1,4)

  def fact (a:Int): Int ={
    product(x => x)(1,a)
  }
  fact(5)

  def mapReduce (f: Int => Int, combine : (Int,Int) => Int, zero : Int) (a: Int, b: Int) : Int =
    if(a > b) zero else combine(f(a), mapReduce(f, combine, zero) (a+1,b))

  mapReduce(x => x, (x,y) => x * y, 1) (1,5)
  mapReduce(x => x, (x,y) => x + y, 0) (1,4)

  val threshold = 0.0001
  def isCloseEnough(a: Double, b : Double) = (abs((a-b)/a)/a) < threshold

  def fixedPoint (f: Double => Double) (firstGuess : Double) : Double = {
    def Iterate (guess : Double) : Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else Iterate(next)
    }
    Iterate(firstGuess)
  }

  def averageDamp(f: Double => Double) (x: Double) = (x + f(x))/2

  def sqrt(x:Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  sqrt(16)

}