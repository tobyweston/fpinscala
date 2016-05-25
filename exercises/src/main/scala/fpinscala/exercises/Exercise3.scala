package fpinscala.exercises

import fpinscala.exercises.Exercise3.curry

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise3 {
  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  // NB: The `Function2` trait has a `curried` method already

}

object Exercise3Test extends App {
  val add: (Int, Int) => Int = (a: Int, b: Int) => a + b
  assert(add(1,1) == 2)

  val curried: (Int) => (Int) => Int = curry(add)
  assert(curried(1)(1) == 2)
}
