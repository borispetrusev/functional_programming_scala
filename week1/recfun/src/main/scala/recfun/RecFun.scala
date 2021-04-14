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
    if (c==0 || c==r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(left: List[Char], bal: Int): Boolean =
      if (bal < 0) false
      else if (left.isEmpty) (bal == 0) else
      if (left.head=='(') loop(left.tail, bal+1)
      else if (left.head==')') loop(left.tail, bal-1)
      else loop(left.tail, bal)
    loop(chars,0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(left: List[Int], sum: Int, n: Int): Int =
      if (sum == 0) 1
      else if (sum < 0 ) 0
      else if (n <= 0 && sum >= 1) 0 else
    count(left.tail, (sum-1), (n-1)) + count(left, (sum-left.head), (n))
    count(coins, money, coins.length)

  }
}
