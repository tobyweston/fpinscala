package fpinscala.exercises

import fpinscala.exercises.Exercise4.uncurry

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise4 {

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

}

object Exercise4Test extends App {

  val add: String => String => String = a => b => a + b
  assert(add("A")("B") == "AB")

  val uncurried: (String, String) => String = uncurry(add)
  assert(uncurried("A", "B") == "AB")

}
