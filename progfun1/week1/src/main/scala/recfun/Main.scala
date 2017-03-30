package recfun

import scala.Char

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
      if(c>r) 0
      if(c==0 || c==r) 1
      else
        pascal(c-1, r-1) + pascal(1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inner(chars: List[Char], acc: Int): Int={
        if(acc < 0 || chars.isEmpty) return acc
        chars.head.toString match{
          case "(" => inner(chars.tail, acc+1)
          case ")" => inner(chars.tail, acc-1)
          case _ => inner(chars.tail, acc)
        }
      }
      inner(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(money == 0) return 1
    if(coins.isEmpty) return 0

    val coin = coins.head

    if(money / coin > 0) countChange(money - coin, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a +1 , f(a) + acc)
    }
    loop(a, 0)
  }
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1 , f(a) * acc)
    }
    loop(a, 1)
  }
}
