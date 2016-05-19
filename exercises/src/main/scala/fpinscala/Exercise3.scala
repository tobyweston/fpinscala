package fpinscala

/**
  * See [[fpinscala.gettingstarted.MyModule]]
  */
object Exercise3 {
  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    ???

  // NB: The `Function2` trait has a `curried` method already

}
