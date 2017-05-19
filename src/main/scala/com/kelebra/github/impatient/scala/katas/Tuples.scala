package com.kelebra.github.impatient.scala.katas

import java.util.Properties

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.language.implicitConversions

/**
  * Chapter 4: Associative arrays and tuples
  */
trait Tuples {

  /**
    * Calculates items with given discount
    *
    * @param `items and prices` mapping of item to its price
    * @param discount           from 0 to 1
    * @return mapping of items to its discounted prices
    */
  def discountedStuff(`items and prices`: Map[String, Double],
                      discount: Double): Map[String, Double]

  /**
    * Counts each word occurrence in given source
    *
    * @param source provider of lines
    * @return mapping of each unique word to its count in a file
    */
  def wordCountIn(source: Source)
                 (implicit order: Ordering[(String, Long)]): Map[String, Long]


  /**
    * Prints out aligned table of java environmental properties and their values
    *
    * @param source of java environmental properties
    * @param out    output of aligned table
    * @param cnv    implicit conversion to scala map
    */
  def prettyPrintJavaVariables[T](source: T)
                                 (out: String => Unit = println)
                                 (implicit cnv: T => Map[String, String]): Unit

  /**
    * Returns minimum and maximum value
    *
    * @param in iterable
    * @tparam T ordered type of elements
    * @return min and max values
    */
  def minMax[T](in: Iterable[T])
               (implicit ord: Ordering[T]): (T, T)

  /**
    * Return number of elements lesser, equal and greater than given one
    *
    * @param in      iterable
    * @param element pivot
    * @tparam T ordered type of elements
    * @return number of elements lesser, equal and greater than given one
    */
  def ltEqGt[T](in: Iterable[T], element: T)
               (implicit ord: Ordering[T]): (Int, Int, Int)
}

object Tuples extends Tuples {

  override def discountedStuff(`items and prices`: Map[String, Double],
                               discount: Double): Map[String, Double] =
    if (discount > 1 || discount < 0) `items and prices`
    else `items and prices`.mapValues(_ * (1 - discount))

  override def wordCountIn(source: Source)
                          (implicit order: Ordering[(String, Long)]): Map[String, Long] = {
    type Stats = Map[String, Long]

    val words: Iterator[String] = source
      .getLines()
      .flatMap(_.split(' ').map(_.trim))

    @tailrec
    def accumulate(acc: Stats = Map.empty.withDefaultValue(0)): Stats =
      if (words.hasNext) {
        val word = words.next().trim
        if(word.nonEmpty) accumulate(acc.updated(word, acc(word) + 1))
        else accumulate(acc)
      }
      else acc

    ListMap(accumulate().toSeq.sorted: _*)
  }

  implicit def javaPropertiesToScalaMap(properties: Properties): Map[String, String] = {
    import collection.JavaConversions._
    mapAsScalaMap(properties).toMap.asInstanceOf[Map[String, String]]
  }

  override def prettyPrintJavaVariables[T](source: T = System.getProperties)
                                          (out: (String) => Unit)
                                          (implicit cnv: T => Map[String, String]): Unit = {
    val properties = cnv(source)
    val max = properties.maxBy(_._1.length)._1.length
    out(
      properties.map { case (k, v) => s"${k.padTo(max, ' ')} | $v" }.mkString("\n")
    )
  }

  override def minMax[T](in: Iterable[T])
                        (implicit ord: Ordering[T]): (T, T) =
    (in.min, in.max)

  override def ltEqGt[T](in: Iterable[T], element: T)
                        (implicit ord: Ordering[T]): (Int, Int, Int) = {
    val comparison = in
      .groupBy(ord.compare(element, _))
      .mapValues(_.size)
      .withDefaultValue(0)
    (comparison(-1), comparison(0), comparison(1))
  }
}
