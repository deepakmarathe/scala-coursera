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
    if (c == 0 || r == c || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head.toString == ")") {
        open > 0 && balance(chars.tail, open - 1)
      }
      else if (chars.head.toString == "(") {
        balance(chars.tail, open + 1)
      }
      else {
        balance(chars.tail, open)
      }
    }

    balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChange(money: Int, coins: List[Int], ways: Int): Int = {
      if (coins.isEmpty) ways
      else if (money - coins.head == 0) ways + 1
      else if (money - coins.head < 0) ways
      else countChange(money - coins.head, coins, ways) + countChange(money, coins.tail, ways)
    }

    countChange(money, coins.sorted, 0)
  }
}
