package fpinscala

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise1 {

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int): Int = {
    def recur(previous: Int, current: Int, n: Int): Int = {
      if (n == 0) previous
      else recur(current, previous + current, n - 1)
    }
    recur(0, 1, n)
  }
}

object TestFib {

  import Exercise1._

  def main(args: Array[String]): Unit = {
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
  }
}
