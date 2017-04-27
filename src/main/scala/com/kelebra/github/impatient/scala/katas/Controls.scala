package com.kelebra.github.impatient.scala.katas

import scala.annotation.tailrec

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

  /**
    * Using this signature write descending loop that starts from 10, ends with 1 and prints all numbers to console
    *
    * @param from start of loop
    * @param to   end of loop (inclusive)
    * @param step of iteration
    * @param out  to console
    * @tparam T numeric type
    */
  def countdown[T](from: T = 10, to: T = 1, step: T = 1)(out: T => Unit = println)(implicit numeric: Numeric[T]): Unit
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

  @tailrec
  override def countdown[T](from: T, to: T, step: T)(out: (T) => Unit)(implicit numeric: Numeric[T]): Unit = {
    import Numeric.Implicits._

    if (numeric.gteq(from, to)) {
      out(from)
      countdown(from - step, to, step)(out)
    }
  }
}
