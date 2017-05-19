package com.kelebra.github.impatient.scala.katas

import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

trait ScalaTestSetup[T] extends WordSpec with Matchers with PropertyChecks with MockFactory {

  protected val eps = 1e-3

  protected val implementation: T
}
