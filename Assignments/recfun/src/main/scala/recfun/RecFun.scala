package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (r == 0)  0
    else pascal(c, r - 1) + pascal(c -1, r -1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], numberOpenParentheses: Int): Boolean = {
      if (chars.isEmpty) numberOpenParentheses == 0
      else if (numberOpenParentheses < 0) false
      else if (chars.head == '(') isBalanced(chars.tail, numberOpenParentheses + 1)
      else if (chars.head == ')') isBalanced(chars.tail, numberOpenParentheses - 1)
      else isBalanced(chars.tail, numberOpenParentheses)
    }
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
