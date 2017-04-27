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
          math.signum(n) shouldBe implementation.signum(n)
        }
      }

      "Double" in {
        forAll { (n: Double) =>
          math.signum(n) shouldBe implementation.signum(n)
        }
      }

      "Float" in {
        forAll { (n: Float) =>
          math.signum(n) shouldBe implementation.signum(n)
        }
      }

      "Int" in {
        forAll { (n: Int) =>
          math.signum(n) shouldBe implementation.signum(n)
        }
      }

      "Short" in {
        forAll { (n: Short) =>
          math.signum(n) shouldBe implementation.signum(n)
        }
      }

      "BigDecimal" in {
        forAll { (n: BigDecimal) =>
          n.signum shouldBe implementation.signum(n)
        }
      }

      "BigInteger" in {
        forAll { (n: BigInt) =>
          n.signum shouldBe implementation.signum(n)
        }
      }
    }
  }
}
