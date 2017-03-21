
def pascal(c: Int, r: Int): Int = {
  if (c < 0 | r < 0) 0 else if (c == 0 & r == 0) 1 else (pascal(c-1,r-1) + pascal(c,r-1))
}

pascal(1,3)

def balance(chars: List[Char]):Boolean = {
  var bal:Int = 0
  for (i <- 0 to (chars.length - 1)) {
    if (chars(i) == '(') bal += 1
    else if (chars(i) == ')') bal -= 1
    if (bal < 0) bal = 99
  }
  if (bal == 0) true else false
}

balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)


def countChange(amount:Int, coins:List[Int]):Int = {
 if (amount == 0) 1
 else if (coins.isEmpty || amount < 0) 0
 else (countChange(amount,coins.tail) + countChange(amount - coins.head,coins))
}

countChange(10,List(2,3,5))

object exercise2 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }
  sum(x => x * x, 3, 4)
}




