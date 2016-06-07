package recfun

import scala.annotation.tailrec

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
    @tailrec
    def loop(r: Int, row: List[Int] = List(1)): Int = {
      if (r == 0) {
        row(c)
      } else {
        val newRow = for{
          i <- (0 to row.size).toList
        } yield {
          if(i == 0 || i == row.size){
            1
          }else {
            row(i-1) + row(i)
          }
        }
        loop(r-1, newRow)
      }
    }
    loop(r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars:List[Char], delta:Int=0):Boolean = {
      if(chars.isEmpty){
        delta == 0
      } else if (delta < 0) {
        false
      } else if(chars.head == '(') {
        loop(chars.tail, delta + 1)
      }else if(chars.head == ')'){
        loop(chars.tail, delta - 1)
      } else {
        loop(chars.tail, delta)
      }
    }
    loop(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val uniqueCoins = coins.toSet

    def loop(testWays:Set[List[Int]], changeWays:Set[List[Int]] = Set()): Int = {
      if(testWays.isEmpty){
        changeWays.size
      } else {
        val(finalWays, notFinalWays) = testWays.partition(l => {
          l.reduceLeft(_ + _) == money
        })
        val unreachedMoneyWays = notFinalWays.filter(l => {
          l.reduceLeft(_ + _) < money
        })
        val newTestWay = for{
          testWay <- unreachedMoneyWays
          coin <- uniqueCoins
        } yield (coin :: testWay).sorted
        loop(newTestWay, finalWays ++ changeWays)
      }
    }
    loop(coins.map(a => List(a)).toSet)
  }
}
