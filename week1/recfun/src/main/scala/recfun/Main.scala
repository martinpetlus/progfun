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
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(str: List[Char], left: Int): Boolean = {
        if (str.isEmpty) if (left == 0) true else false
        else if (str.head == '(') helper(str.tail, left + 1)
        else if (str.head == ')') if (left > 0) helper(str.tail, left - 1) else false
        else helper(str.tail, left)
      }

      helper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def helper(m: Int, c: List[Int]): Int =
        if (m == 0) 1
        else if (m < 0 || c.isEmpty) 0
        else helper(m - c.head, c) + helper(m, c.tail)

      if (money == 0 || coins.isEmpty) 0
      else helper(money, coins)
    }
  }
