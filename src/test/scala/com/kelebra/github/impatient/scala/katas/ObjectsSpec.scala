package com.kelebra.github.impatient.scala.katas

import java.awt.Color

import com.kelebra.github.impatient.scala.katas.CardSuit._
import com.kelebra.github.impatient.scala.katas.UnitConversion._
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class ObjectsSpec extends WordSpec with Matchers {

  "Inches" should {

    "be created with implicit conversion" in {
      val inches: Inches = 100
      inches.value shouldBe 100
    }

    "convert to meters" in {
      val inches: Inches = 100
      (inches toMeters).value shouldBe 2.54
    }

    "convert to miles" in {
      val inches: Inches = 100
      (inches toMiles).value shouldBe 0.001578
    }
  }

  "Meters" should {

    "be created with implicit conversion" in {
      val meters: Meters = 100
      meters.value shouldBe 100
    }

    "convert to inches" in {
      val meters: Meters = 100
      (meters toInches).value shouldBe 3937.00787400
    }

    "convert to miles" in {
      val meters: Meters = 100
      (meters toMiles).value shouldBe 0.06213700
    }
  }

  "Miles" should {

    "be created with implicit conversion" in {
      val miles: Miles = 100
      miles.value shouldBe 100
    }

    "convert to inches" in {
      val miles: Miles = 100
      (miles toInches).value shouldBe 6.336e+6
    }

    "convert to meters" in {
      val miles: Miles = 100
      (miles toMeters).value shouldBe 160934.709
    }
  }

  "Inter conversion" should {

    val Eps = 1e-3

    "be transitive for meter through miles" in {
      val meters: Meters = 100

      meters.toMiles.toMeters.value should be(meters.value +- Eps)
    }

    "be transitive for inches through meters" in {
      val inches: Inches = 100

      inches.toMeters.toInches.value should be(inches.value +- Eps)
    }
  }

  "Card suits colors" should {

    "have spades and clubs of black color" in {

      CardSuit.colorOf(♠) shouldBe Color.BLACK
      CardSuit.colorOf(♣) shouldBe Color.BLACK
    }

    "have heart and diamond of red color" in {

      CardSuit.colorOf(♥) shouldBe Color.RED
      CardSuit.colorOf(♦) shouldBe Color.RED
    }
  }
}
