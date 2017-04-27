package com.kelebra.github.impatient.scala.katas

/**
  * Chapter 2: Control Structures and Functions
  */
trait Controls {

  /**
    * Calculates signum function such that:
    * <br/>-1, if n < 0
    * <br/> 0, if n = 0
    * <br/> 1, if n > 0
    *
    * @param n numeric parameter (int, double etc)
    * @tparam T numeric type
    */
  def signum[T](n: T)(implicit numeric: Numeric[T]): Int
}

/**
  * Chapter 2: Answers
  */
object Controls extends Controls {

  override def signum[T](n: T)(implicit numeric: Numeric[T]): Int = {
    val zero = numeric.fromInt(0)

    if (numeric.lt(n, zero)) -1
    else if (numeric.gt(n, zero)) 1
    else 0
  }
}
