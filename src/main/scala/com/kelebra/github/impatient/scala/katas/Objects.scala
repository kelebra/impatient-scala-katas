package com.kelebra.github.impatient.scala.katas

import java.awt.Color

import scala.language.implicitConversions

/**
  * For given units provide convenient conversion and creation methods
  */

case class Inches(value: BigDecimal) extends AnyVal

case class Meters(value: BigDecimal) extends AnyVal

case class Miles(value: BigDecimal) extends AnyVal

object Inches {

  implicit def fromBigDecimalView[T](v: T)
                                    (implicit cnv: T => BigDecimal): Inches =
    Inches(cnv(v))
}

object Meters {

  implicit def fromBigDecimalView[T](v: T)
                                    (implicit cnv: T => BigDecimal): Meters =
    Meters(cnv(v))
}

object Miles {

  implicit def fromBigDecimalView[T](v: T)
                                    (implicit cnv: T => BigDecimal): Miles =
    Miles(cnv(v))
}

object UnitConversion {

  implicit class InchesConversion(inches: Inches) {

    def toMeters: Meters = inches.value * 0.0254

    def toMiles: Miles = inches.value * 1.578E-5
  }

  implicit class MetersConversion(meters: Meters) {

    def toInches: Inches = meters.value * 39.37007874

    def toMiles: Miles = meters.value * 0.00062137
  }

  implicit class MilesConversion(miles: Miles) {

    def toInches: Inches = miles.value * 63360

    def toMeters: Miles = miles.value * 1609.34709
  }

}

/**
  * Create enum card suits and implement color identification
  */
object CardSuit extends Enumeration {

  val ♠, ♥, ♦, ♣ = Value

  def colorOf(cardSuit: CardSuit.Value): Color =
    cardSuit match {
      case `♠` | `♣` => Color.BLACK
      case `♥` | `♦` => Color.RED
    }
}