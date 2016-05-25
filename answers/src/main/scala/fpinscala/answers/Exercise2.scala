package fpinscala.answers

import fpinscala.answers.Exercise2.isSorted

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise2 {
  // Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

}

object Exercise2Test extends App {
  assert(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => b > a))
  assert(isSorted(Array(1), (a: Int, b: Int) => b > a))
  assert(!isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => b < a))
  assert(!isSorted(Array(1, 3, 3, 4, 5), (a: Int, b: Int) => b > a))
}
