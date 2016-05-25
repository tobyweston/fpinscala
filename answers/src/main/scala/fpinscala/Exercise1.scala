package fpinscala

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise1 {

  // Exercise 1: Write a function to compute the nth fibonacci number

  // 0 and 1 are the first two numbers in the sequence,
  // so we start the accumulators with those.
  // At every iteration, we add the two numbers to get the next one.
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

}

object TestFib {

  import Exercise1._

  def main(args: Array[String]): Unit = {
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
  }
}
