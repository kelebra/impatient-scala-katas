package com.kelebra.github.impatient.scala.katas

import org.scalacheck.Gen

import scala.collection.immutable.ListMap
import scala.io.Source

class TuplesSpec extends ScalaTestSetup[Tuples] {

  val implementation = Tuples

  "Tuples And Associative Arrays" when {

    "method discountedStuff implemented" should {

      "work for empty mapping" in {
        forAll { discount: Int =>
          implementation.discountedStuff(Map.empty, discount) shouldBe Map.empty
        }
      }

      "work for multiple items and 10% discount" in {
        implementation.discountedStuff(Map(
          "book" -> 44.99,
          "gold" -> 100.05,
          "camera" -> 1505.80
        ), 0.1) shouldBe Map(
          "book" -> 40.491,
          "gold" -> 90.045,
          "camera" -> 1355.22
        )
      }

      "not change prices if percent is negative" in {
        forAll(Gen.choose(Double.MinValue, -0.1)) { discount: Double =>
          val mapping = Map(
            "book" -> 44.99,
            "gold" -> 100.05,
            "camera" -> 1505.80
          )
          implementation.discountedStuff(mapping, discount) shouldBe mapping
        }
      }

      "not change prices if percent is more 100%" in {
        forAll(Gen.choose(1.1, Double.MaxValue)) { discount: Double =>
          val mapping = Map(
            "book" -> 44.99,
            "gold" -> 100.05,
            "camera" -> 1505.80
          )
          implementation.discountedStuff(mapping, discount) shouldBe mapping
        }
      }

      "change prices for any valid percent" in {
        forAll(Gen.choose(0.0, 1.0)) { discount: Double =>
          val initial = Map(
            "book" -> 44.99,
            "gold" -> 100.05,
            "camera" -> 1505.80
          )
          val discounted = implementation.discountedStuff(initial, discount)
          discounted should not be initial
        }
      }
    }

    "method wordCountIn is implemented" should {

      "work for empty file" in {
        implementation.wordCountIn(Source.fromString("")) shouldBe empty
      }

      "work for simple file" in {
        val lines = Source.fromInputStream(getClass.getResourceAsStream("/sample.txt"))
        implementation.wordCountIn(lines) shouldBe Map(
          "add" -> 1,
          "as" -> 1,
          "file" -> 1,
          "guys" -> 1,
          "hello" -> 2,
          "is" -> 1,
          "just" -> 1,
          "oh" -> 1,
          "sample" -> 3,
          "this" -> 1,
          "we" -> 1,
          "will" -> 1,
          "word" -> 1
        )
      }

      "work for with key ordering" in {
        implicit val order = (k: String, _: Long) => k
        implementation.wordCountIn(
          Source.fromString(
            """
              |zed
              |marvel
              |ast
            """.stripMargin)
        ) shouldBe ListMap("ast" -> 1, "marvel" -> 1, "zed" -> 1)
      }

      "work for with value ordering" in {
        implicit val order = (_: String, v: Long) => v
        implementation.wordCountIn(
          Source.fromString(
            """
              |zed
              |marvel
              |zed
            """.stripMargin)
        ) shouldBe ListMap("zed" -> 2, "marvel" -> 1)
      }
    }

    "method prettyPrintJavaVariables implemented" should {

      "work on current machine" in {
        import Tuples.javaPropertiesToScalaMap

        var out = ""
        implementation.prettyPrintJavaVariables()(result => out = result)

        out should not be empty
        out should include("|")
        out should include("\n")

        assert({
          val lines = out
            .split("\n")
            .filter(_.nonEmpty)

          lines.forall(line => line.contains("|")) && lines.map(_.split("|")(0).length).distinct.length == 1
        })
      }
    }

    "method minMax implemented" should {

      "work for simple list" in {
        implementation.minMax(
          3 :: 2 :: 1 :: 523 :: 1 :: 1 ::
            3 :: 42 :: 7 :: 89 :: 976 :: Nil
        ) shouldBe ((1, 976))
      }

      "work for single element list" in {
        implementation.minMax(1 :: Nil) shouldBe ((1, 1))
      }

      "not work for empty list" in {
        assertThrows[UnsupportedOperationException] {
          implementation.minMax(List.empty[Int]) shouldBe ((1, 1))
        }
      }
    }

    "method ltEqGt implemented" should {

      "work for empty list" in {
        implementation.ltEqGt(List.empty, 5) shouldBe ((0, 0, 0))
      }

      "work for simple list" in {
        implementation.ltEqGt(1 :: 2 :: 3 :: 4 :: 5 :: Nil, 3) shouldBe ((2, 1, 2))
      }
    }
  }
}
