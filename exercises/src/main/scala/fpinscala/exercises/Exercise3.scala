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
    a => b => f(a,b)
  }

  // NB: The `Function2` trait has a `curried` method already

}

object Exercise3Test extends App {
  val add: (String, String) => String = (a: String, b: String) => a + b
  assert(add("A", "B") == "AB")

  val curried: (String) => (String) => String = curry(add)
  assert(curried("A")("B") == "AB")
}
