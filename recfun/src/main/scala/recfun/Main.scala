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
      if (c==0 || c==r) 1 else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val open_par: Char = '('
      val close_par: Char = ')'

      def f(cstr: List[Char], count: Int): Boolean = {
        if (cstr.isEmpty) count == 0
        else if (count < 0) false
        else {
          val c: Char = cstr.head

          if (c == open_par) f(cstr.tail, count + 1)
          else if (c == close_par) f(cstr.tail, count - 1)
          else f(cstr.tail, count)
        }
      }

      f(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def f(m: Int, change: List[Int], count: Int): Int = {
        if (m < 0) count
        else
          if (change.isEmpty)
             if (m == 0) count + 1 else count
          else
            f(m - change.head, change, count) + f(m, change.tail, count)
      }
      f(money, coins, 0)
    }
  }
