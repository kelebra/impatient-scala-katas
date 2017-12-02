package com.kelebra.github.impatient.scala.katas

import scala.language.implicitConversions

/**
  * Chapter 5: Classes
  */

/**
  * Extend class Counter to eliminate integer overflow
  */
class Counter {

  protected var value = 0

  def increment(): Unit = value += 1

  def current(): Int = value
}

/**
  * Write class BankAccount:
  * - property balance
  * - method deposit
  * - method withdraw
  *
  * available for read only.
  */
trait Account[T] {

  def deposit[A](amount: A)(implicit cnv: A => T): Unit

  def withdraw[A](amount: A)(implicit cnv: A => T): Unit

  def balance: T
}

/**
  * Implement two time classes. One of them should store time in 24-hr format
  * and another one as minutes from start of the day. Public API may not change.
  */
trait Time extends Ordered[Time] {

  def hours: HourOfDay

  def minutes: MinuteOfDay

  def before(other: Time): Boolean = this < other

  override def compare(other: Time): Int =
    if (hours == other.hours) minutes.value - other.minutes.value
    else hours.value - other.hours.value
}

case class HourOfDay(value: Int) {

  if (value < 0 || value > 23)
    throw new IllegalStateException(s"Invalid hour value $value")
}

case class MinuteOfDay(value: Int) {

  if (value < 0 || value > 59)
    throw new IllegalStateException(s"Invalid minute value $value")
}

object Classes {

  /**
    * Solution to counter task
    */

  type OverflowStrategy[T] = T => T

  trait StartOverStrategy extends OverflowStrategy[Int] {def apply(v1: Int): Int = 0}

  trait ThrowExceptionStrategy extends OverflowStrategy[Int] {
    def apply(v1: Int): Int = throw new IllegalStateException("Overflow happened")
  }

  trait NonOverflowCounter extends Counter {
    this: OverflowStrategy[Int] =>

    override def increment(): Unit = {
      if (value < 0) value = apply(value)
      else super.increment()
    }
  }

  object CycledCounter extends NonOverflowCounter with StartOverStrategy

  object ThrowingCounter extends NonOverflowCounter with ThrowExceptionStrategy

  /**
    * Solution to Account class
    */

  class BankingAccount[T](private var _balance: T)(implicit val num: Numeric[T]) extends Account[T] {

    override def balance: T = _balance

    override def deposit[A](amount: A)(implicit cnv: A => T): Unit =
      _balance = num.plus(_balance, amount)

    override def withdraw[A](amount: A)(implicit cnv: A => T): Unit =
      if (num.gt(amount, _balance)) throw new IllegalStateException("Not allowed")
      else _balance = num.minus(_balance, cnv(amount))
  }

  /**
    * Solution to time classes
    */

  implicit def int2Hour(i: Int): HourOfDay = HourOfDay(i)

  implicit def int2Minute(i: Int): MinuteOfDay = MinuteOfDay(i)

  case class Time24(hours: HourOfDay, minutes: MinuteOfDay) extends Time

  case class MinutesSinceStartOfDay(private val _sodMinutes: Int) extends Time {

    override def hours: HourOfDay = _sodMinutes / 60

    override def minutes: MinuteOfDay = _sodMinutes % 60
  }

}