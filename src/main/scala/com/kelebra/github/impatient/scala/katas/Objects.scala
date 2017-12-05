package com.kelebra.github.impatient.scala.katas

import java.awt.Color

import scala.language.{implicitConversions, postfixOps}

/**
  * For given units provide convenient conversion and creation methods
  */

case class Inches(value: BigDecimal) extends AnyVal

case class Meters(value: BigDecimal) extends AnyVal

case class Miles(value: BigDecimal) extends AnyVal

object LengthUnitsImplicits {

  implicit class ViewToLengthUnit[T](v: T)(implicit cnv: T => BigDecimal) {

    def inches = Inches(v)

    def meters = Meters(v)

    def miles = Miles(v)
  }

}

object UnitConversion {

  import LengthUnitsImplicits.ViewToLengthUnit

  implicit class InchesConversion(inches: Inches) {

    def toMeters: Meters = (inches.value * 0.0254) meters

    def toMiles: Miles = (inches.value * 1.578E-5) miles
  }

  implicit class MetersConversion(meters: Meters) {

    def toInches: Inches = (meters.value * 39.37007874) inches

    def toMiles: Miles = (meters.value * 0.00062137) miles
  }

  implicit class MilesConversion(miles: Miles) {

    def toInches: Inches = (miles.value * 63360) inches

    def toMeters: Meters = (miles.value * 1609.34709) meters
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