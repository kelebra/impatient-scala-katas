package com.kelebra.github.impatient.scala.katas

import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class ControlsSpec extends WordSpec with Matchers with PropertyChecks with MockFactory {

  lazy val implementation: Controls = Controls
  private val eps = 1e-3

  private def `work for type` = afterWord("work for type")

  "Control Structures and Functions" when {

    "signum method implemented, it" should `work for type` {

      "Long" in {
        forAll { (n: Long) =>
          implementation.signum(n) shouldBe math.signum(n)
        }
      }

      "Double" in {
        forAll { (n: Double) =>
          implementation.signum(n) shouldBe math.signum(n)
        }
      }

      "Float" in {
        forAll { (n: Float) =>
          implementation.signum(n) shouldBe math.signum(n)
        }
      }

      "Int" in {
        forAll { (n: Int) =>
          implementation.signum(n) shouldBe math.signum(n)
        }
      }

      "Short" in {
        forAll { (n: Short) =>
          implementation.signum(n) shouldBe math.signum(n)
        }
      }

      "BigDecimal" in {
        forAll { (n: BigDecimal) =>
          implementation.signum(n) shouldBe n.signum
        }
      }

      "BigInteger" in {
        forAll { (n: BigInt) =>
          implementation.signum(n) shouldBe n.signum
        }
      }
    }

    "countdown method called with default values" should {

      "print all values from 10 to 1 inclusive" in {
        val out = mockFunction[Int, Unit]
        inSequence {
          out expects 10
          out expects 9
          out expects 8
          out expects 7
          out expects 6
          out expects 5
          out expects 4
          out expects 3
          out expects 2
          out expects 1
        } returning Unit
        implementation.countdown()(out)
      }
    }

    "both product(Recursive) implemented" should {

      "return the same results" in {
        forAll { (in: String) =>
          implementation.product(in) shouldBe implementation.productRecursive(in)
        }
      }
    }

    "implemented product method" should {

      "return 1 for empty string" in {
        implementation.product("") shouldBe 1
      }

      "return 9415087488 for 'Hello'" in {
        implementation.product("Hello") shouldBe 9415087488L
      }
    }

    "pow method implemented" should `work for type` {

      "odd positive integer" in {
        implementation.pow(2, 15) shouldBe math.pow(2, 15)
      }

      "even positive integer" in {
        implementation.pow(2, 14) shouldBe math.pow(2, 14)
      }

      "any integer" in {
        forAll { (x: Int, n: Int) =>
          implementation.pow(x, n) shouldBe math.pow(x, n) +- eps
        }
      }
    }
  }
}
