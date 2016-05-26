package fpinscala.exercises

import fpinscala.exercises.Exercise5.compose

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise5 {
  // Exercise 5: Implement `compose`

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}

object Exercise5Test extends App {
  val plus2: Int => Int = a => a + 2
  val multiplyBy2: Int => Int = a => a * 2

  val f    = (x: Double) => math.Pi / 2 - x
  val sin  = (x: Double) => java.lang.Math.sin(x)

  val cos = sin.compose(f)

  assert(compose(plus2, multiplyBy2)(5) == 12)
  assert(compose(multiplyBy2, plus2)(5) == 14)

  // non associative test
  assert(plus2.andThen(multiplyBy2)(5) == multiplyBy2.compose(plus2)(5), "functions are associative")
  assert(multiplyBy2.andThen(plus2)(5) != multiplyBy2.compose(plus2)(5), "functions are not associative")

  assert(compose(multiplyBy2, plus2)(5) == multiplyBy2.compose(plus2)(5))
  assert(compose(multiplyBy2, plus2)(5) == plus2.andThen(multiplyBy2)(5))

//  assert(cos(100) == Math.cos(100))
}