package fpinscala

import fpinscala.Exercise2._

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise2 {
  // Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted

  def isSortedAlt[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def recur(n: Int): Boolean = {
      if (n == 1) true
      else if (!ordered(array(n - 1), array(n))) false
      else recur(n - 1)
    }

    recur(array.length - 1)
  }

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def recur(list: List[A]): Boolean = list match {
      case Nil                                                => true
      case first :: second :: tail if !ordered(first, second) => false
      case _ :: tail                                          => recur(tail)
    }

    recur(array.toList)
  }

  // inspired by this function
  def findFirst[A](array: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def recur(n: Int): Int = {
      if (n >= array.length) -1
      else if (p(array(n))) n
      else recur(n + 1)
    }

    recur(0)
  }
}

object Exercise2Test extends App {
  println(findFirst(Array(1, 2, 3, 2, 5), (a: Int) => a > 3) + " position")

  assert(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => b > a))
  assert(isSorted(Array(1), (a: Int, b: Int) => b > a))
  assert(!isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => b < a))
  assert(!isSorted(Array(1, 3, 3, 4, 5), (a: Int, b: Int) => b > a))
}
