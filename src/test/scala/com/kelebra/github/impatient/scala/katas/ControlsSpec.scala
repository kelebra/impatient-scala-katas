package com.kelebra.github.impatient.scala.katas

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class ControlsSpec extends WordSpec with Matchers with PropertyChecks {

  lazy val implementation: Controls = Controls

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
  }
}
