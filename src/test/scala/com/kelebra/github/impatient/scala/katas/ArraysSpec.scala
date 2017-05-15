package com.kelebra.github.impatient.scala.katas

import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer

class ArraysSpec extends WordSpec with Matchers with PropertyChecks with MockFactory {

  val implementation: Arrays = Arrays

  "Control Structures and Functions" when {

    "method range implemented" should {

      "work for integers" in {
        forAll(Gen.choose(0, 1000)) { (n: Int) =>
          implementation.range(n) shouldBe (0 until n).toArray
        }
      }
    }

    "method swapAdjacentElements implemented" should {

      "not change empty array" in {
        val array = Array.empty
        implementation.swapAdjacentElements(array)
        array shouldBe Array.empty
      }

      "not change array of size one" in {
        val array = Array(1)
        implementation.swapAdjacentElements(array)
        array shouldBe Array(1)
      }

      "change odd size array" in {
        val array = Array(1, 2, 3, 4, 5)
        implementation.swapAdjacentElements(array)
        array shouldBe Array(2, 1, 4, 3, 5)
      }

      "change even size array" in {
        val array = Array(1, 2, 3, 4, 5, 6)
        implementation.swapAdjacentElements(array)
        array shouldBe Array(2, 1, 4, 3, 6, 5)
      }
    }

    "methods swapAdjacentElements and swappedAdjacentElements are implemented" should {

      "return the same values" in {
        forAll { (array: Array[Int]) =>
          whenever(array.length <= 100) {
            val swapped = implementation.swappedAdjacentElements(array)
            implementation.swapAdjacentElements(array)
            swapped shouldBe array
          }
        }
      }
    }

    "method positiveFirstWithOriginalOrder is implemented" should {

      "work for all arrays" in {
        forAll { (array: Array[Int]) =>
          whenever(array.length <= 100) {
            implementation.positiveFirstWithOriginalOrder(array) shouldBe
              (array.filter(_ > 0) ++ array.filterNot(_ > 0))
          }
        }
      }
    }

    "method avg is implemented" should {

      "work for trivial array" in {
        implementation.avg(Array(10, -5, 15, 25, 40)) shouldBe 17.0
      }

      "work for empty array" in {
        implementation.avg(Array()).isNaN
      }
    }

    "method sortInReverseOrder is implemented" should {

      "work for trivial array" in {
        val array = ArrayBuffer(10, -5, 15, 25, 40)
        implementation.sortedInReverseOrder(array)
        array shouldBe Array(40, 25, 15, 10, -5)
      }
    }

    "method deduplicate is implemented" should {

      "work for any array" in {
        val out = mockFunction[Int, Unit]
        inSequence {
          out expects 1
          out expects 2
          out expects 3
          out expects 4
          out expects 5
        } returning Unit
        implementation.deduplicate(Array(2, 2, 2, 2, 1, 1, 5, 5, 1, 1, 3, 3, 3, 4), out)
      }
    }
  }
}
