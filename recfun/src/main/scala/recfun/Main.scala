package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 | r < 0) 0 else if (c == 0 & r == 0) 1 else (pascal(c-1,r-1) + pascal(c,r-1))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]):Boolean = {
    var bal:Int = 0
    for (i <- 0 to (chars.length - 1)) {
      if (chars(i) == '(') bal += 1
      else if (chars(i) == ')') bal -= 1
      if (bal < 0) bal = 99
    }
    if (bal == 0) true else false
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else (countChange(money,coins.tail) + countChange(money - coins.head,coins))
  }
}