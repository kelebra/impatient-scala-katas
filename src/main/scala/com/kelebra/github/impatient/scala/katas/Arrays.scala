package com.kelebra.github.impatient.scala.katas

import java.util.TimeZone

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait Arrays {

  /**
    * Creates array of elements from 0 inclusive to n exclusive
    *
    * @param n upper bound
    * @tparam T integral type
    */
  def range[T: ClassTag](n: T)(implicit integral: Integral[T]): Array[T]

  /**
    * Swaps adjacent elements in array
    *
    * @param array of elements
    * @tparam T element type
    */
  def swapAdjacentElements[T](array: Array[T]): Unit

  /**
    * Returns new array with swapped adjacent elements
    *
    * @param array of elements
    * @tparam T element type
    * @return array with swapped adjacent elements
    */
  def swappedAdjacentElements[T: ClassTag](array: Array[T]): Array[T]

  /**
    * Creates new array where positive numbers come first and negative with zeroes afterwards in original order
    *
    * @param array of elements
    * @tparam T integral element type
    * @return new array where positive numbers come first and negative with zeroes afterwards in original order
    */
  def positiveFirstWithOriginalOrder[T: ClassTag](array: Array[T])(implicit integral: Integral[T]): Array[T]

  /**
    * Computes average of array elements
    *
    * @param array of elements
    * @return average value
    */
  def avg(array: Array[Double]): Double

  /**
    * Sorted array in reversed order
    *
    * @param array of elements
    * @tparam T element type
    */
  def sortedInReverseOrder[T](array: ArrayBuffer[T])(implicit ord: Ordering[T]): Unit

  /**
    * Outputs all unique values in array
    *
    * @param array of elements
    * @param out   output function
    * @tparam T element type
    */
  def deduplicate[T](array: Array[T], out: T => Unit)(implicit ord: Ordering[T]): Unit

  /**
    * Computes all american timezones
    *
    * @return identifiers of all american timezones
    */
  def allAmericanTimezones: Array[String]
}

object Arrays extends Arrays {

  import Integral.Implicits._
  import Ordering.Implicits._

  override def range[T: ClassTag](n: T)(implicit integral: Integral[T]): Array[T] = {
    @tailrec
    def fill(index: T, result: Array[T]): Array[T] =
      if (index >= n) result
      else {
        result(integral.toInt(index)) = index
        fill(index + integral.fromInt(1), result)
      }

    fill(integral.fromInt(0), Array.ofDim[T](integral.toInt(n)))
  }

  private def swap[T](array: Array[T], i: Int, j: Int): Unit = {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp
  }

  override def swapAdjacentElements[T](array: Array[T]): Unit =
    if (array.nonEmpty)
      for {i <- 1 until(array.length, 2)}
        swap(array, i, i - 1)

  override def swappedAdjacentElements[T: ClassTag](array: Array[T]): Array[T] =
    array
      .grouped(2)
      .flatMap {
        case Array(a, b)   => Array(b, a)
        case Array(single) => Array(single)
      }
      .toArray[T]

  override def positiveFirstWithOriginalOrder[T: ClassTag](array: Array[T])(implicit integral: Integral[T]): Array[T] = {

    val destination: Array[T] = Array.ofDim(array.length)

    def copyValues(i: Int, write: Int)(condition: T => Boolean): Int = {
      if (i == array.length) write
      else if (condition(array(i))) {
        destination(write) = array(i)
        copyValues(i + 1, write + 1)(condition)
      }
      else copyValues(i + 1, write)(condition)
    }

    copyValues(0, copyValues(0, 0) {_ > integral.fromInt(0)}) {_ <= integral.fromInt(0)}
    destination
  }

  override def avg(array: Array[Double]): Double =
    if (array.isEmpty) Double.NaN
    else array.sum / array.length

  override def sortedInReverseOrder[T](array: ArrayBuffer[T])(implicit ord: Ordering[T]): Unit = {
    import collection.JavaConverters._
    java.util.Collections.sort(array.asJava, ord.reverse)
  }

  override def deduplicate[T](array: Array[T], out: T => Unit)(implicit ord: Ordering[T]): Unit = {

    @tailrec
    def skippingNonUnique(i: Int, in: Array[T], last: Option[T] = None): Unit = {
      if (i < in.length) {
        val element = in(i)

        if (last.contains(element)) skippingNonUnique(i + 1, in, last)
        else {
          out(element)
          skippingNonUnique(i + 1, in, Option(element))
        }
      }
    }

    skippingNonUnique(0, array.sorted)
  }

  override def allAmericanTimezones: Array[String] =
    TimeZone.getAvailableIDs
      .withFilter(_.startsWith("America/"))
      .map(_.stripPrefix("America/"))
      .sorted
}
